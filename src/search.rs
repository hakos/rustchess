use super::board;

const MAX_PV_DEPTH: usize = 64;
const MATE_SCORE: i32 = 10000;

pub struct PrincipalVariation {
    moves: [Option<board::Move>; MAX_PV_DEPTH],
    num_moves: usize,
}

impl PrincipalVariation {
    pub fn cleared() -> PrincipalVariation {
        PrincipalVariation {
            moves: [None; MAX_PV_DEPTH],
            num_moves: 0,
        }
    }

    pub fn at(&self, index: usize) -> board::Move {
        assert!(index < self.num_moves);
        self.moves[index].unwrap()
    }

    pub fn set_moves(&mut self, first: board::Move, rest: &PrincipalVariation) {
        assert!(rest.num_moves < MAX_PV_DEPTH);
        self.moves[0] = Some(first);
        self.moves[1..=rest.num_moves].copy_from_slice(&rest.moves[0..rest.num_moves]);
        self.num_moves = rest.num_moves + 1;
        for i in self.num_moves..MAX_PV_DEPTH {
            self.moves[i] = None
        }
    }
}

pub fn perft(board: &board::Board, depth: u32, debug: bool) -> u64 {
    let mut num_nodes = 0;

    if depth == 0 {
        return 1;
    }

    // Pseudo legal moves may leave us checked
    for m in board.pseudo_legal_move_iter() {
        let mut board_copy = *board;
        board_copy.make_move_unverified(m);
        if !board_copy.is_checked() {
            board_copy.change_turn();
            let num = perft(&board_copy, depth - 1, false);
            if debug {
                println!("{}: {}", m, num);
            }
            num_nodes += num;
        }
    }

    num_nodes
}

pub fn negamax_impl(
    board: &board::Board,
    depth: u32,
    ply: u32,
    mut alpha: i32,
    beta: i32,
    prev_move: Option<board::Move>,
    is_checked: bool,
    pv: &mut PrincipalVariation,
    first_moves: &[Option<board::Move>],
) -> i32 {
    // Reference: https://www.chessprogramming.org/Alpha-Beta

    if depth == 0 {
        pv.num_moves = 0;
        return board.evaluate();
    }

    let mut children_pv = PrincipalVariation::cleared();
    let mut any_legal_moves = false;

    let rest_moves = board
        .pseudo_legal_move_iter()
        .filter(|&m| Some(m) != first_moves[0]);

    for m in first_moves[0].into_iter().chain(rest_moves) {
        let mut board_copy = *board;
        board_copy.make_move_unverified(m);
        if !board_copy.is_checked() {
            any_legal_moves = true;
            board_copy.change_turn();
            let is_opponent_checked = board_copy.is_checked();
            let new_depth = if depth == 1
                && ((prev_move.is_some() && m.dst == prev_move.unwrap().dst)
                    || is_opponent_checked)
            {
                // Position is not "quiet" at leaf node; extend depth to
                // resolve captures and checks with a "quiescence search"
                1
            } else {
                depth - 1
            };
            let score = -negamax_impl(
                &board_copy,
                new_depth,
                ply + 1,
                -beta,
                -alpha,
                Some(m),
                is_opponent_checked,
                &mut children_pv,
                if Some(m) == first_moves[0] { &first_moves[1..] } else { &[None] },
            );
            if score >= beta {
                return beta; // fail hard beta-cutoff
            }
            if score > alpha {
                alpha = score; // alpha acts like max in MiniMax
                pv.set_moves(m, &children_pv)
            }
        }
    }

    if !any_legal_moves {
        let score = if is_checked {
            // Mate: add ply to prefer mate in fewer moves
            -MATE_SCORE + ply as i32
        } else {
            // Stale mate
            0
        };
        if score >= beta {
            return beta;
        }
        if score > alpha {
            alpha = score;
            pv.num_moves = 0;
        }
    }

    alpha
}

pub fn negamax(
    board: &board::Board,
    depth: u32,
    debug: bool,
    first_moves: &[Option<board::Move>],
) -> (i32, PrincipalVariation) {
    assert!(depth > 0);

    let alpha = -MATE_SCORE;
    let beta = MATE_SCORE;
    let mut pv = PrincipalVariation::cleared();

    let score = negamax_impl(
        board,
        depth,
        0,
        alpha,
        beta,
        None,
        board.is_checked(),
        &mut pv,
        first_moves,
    );

    (score, pv)
}

pub fn negamax_iterative_deepening(
    board: &board::Board,
    time_budget: std::time::Duration,
) -> (i32, PrincipalVariation, u32) {
    let start = std::time::Instant::now();
    let mut depth = 1;
    let mut pv = PrincipalVariation::cleared();

    loop {
        let iteration_start = std::time::Instant::now();

        let (best_score, moves) = negamax(board, depth, false, &pv.moves);
        pv = moves;

        let estimated_branching_factor = board.count_pieces();
        let estimated_next_iteration_time =
            iteration_start.elapsed() * estimated_branching_factor;

        let time = iteration_start.elapsed();
        println!(
            "info depth {} score cp {} time {} pv {}",
            depth,
            best_score,
            time.as_secs() * 1_000u64 + time.subsec_micros() as u64 / 1_000u64,
            pv.moves
                .iter()
                .take(pv.num_moves)
                .map(|m| format!("{}", m.unwrap()))
                .collect::<Vec<String>>()
                .join(" "),
        );

        if start.elapsed() + estimated_next_iteration_time / 2 > time_budget {
            break (best_score, pv, depth);
        }

        depth += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::board::Board;

    // https://gist.github.com/peterellisjones/8c46c28141c162d1d8a0f0badbc9cff9

    #[test]
    fn perft_test_position_1() {
        let board = Board::from_fen("r6r/1b2k1bq/8/8/7B/8/8/R3K2R b QK");
        assert_eq!(8, board.count_moves());
        assert_eq!(8, perft(&board, 1, false));
    }

    #[test]
    fn perft_test_position_2() {
        let board = Board::from_fen("8/8/8/2k5/2pP4/8/B7/4K3 b - d3");
        assert_eq!(8, board.count_moves());
        assert_eq!(8, perft(&board, 1, false));
    }

    #[test]
    fn perft_test_position_3() {
        let board = Board::from_fen("r1bqkbnr/pppppppp/n7/8/8/P7/1PPPPPPP/RNBQKBNR w QqKk");
        assert_eq!(19, board.count_moves());
        assert_eq!(19, perft(&board, 1, false));
    }

    #[test]
    fn peft_test_position_4() {
        let board = Board::from_fen("r3k2r/p1pp1pb1/bn2Qnp1/2qPN3/1p2P3/2N5/PPPBBPPP/R3K2R b QqKk");
        assert_eq!(5, board.count_moves());
        assert_eq!(5, perft(&board, 1, false));
    }

    #[test]
    fn peft_test_position_5() {
        let board = Board::from_fen("2kr3r/p1ppqpb1/bn2Qnp1/3PN3/1p2P3/2N5/PPPBBPPP/R3K2R b QK");
        assert_eq!(44, board.count_moves());
        assert_eq!(44, perft(&board, 1, false));
    }

    #[test]
    fn peft_test_position_6() {
        let board = Board::from_fen("rnb2k1r/pp1Pbppp/2p5/q7/2B5/8/PPPQNnPP/RNB1K2R w QK");
        assert_eq!(39, board.count_moves());
        assert_eq!(39, perft(&board, 1, false));
    }

    #[test]
    fn peft_test_position_7() {
        let board = Board::from_fen("2r5/3pk3/8/2P5/8/2K5/8/8 w -");
        assert_eq!(9, board.count_moves());
        assert_eq!(9, perft(&board, 1, false));
    }

    #[test]
    fn perft_test_position_8() {
        let board = Board::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ -");
        assert_eq!(62379, perft(&board, 3, true));
    }

    #[test]
    fn perft_test_position_9() {
        let board = Board::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ -");
        assert_eq!(62379, perft(&board, 3, true));
    }

    #[test]
    fn perft_test_position_10() {
        let board =
            Board::from_fen("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - -");
        assert_eq!(89890, perft(&board, 3, true));
    }

    #[test]
    fn perft_test_position_11() {
        let board = Board::from_fen("K1k5/8/P7/8/8/8/8/8 w - -");
        assert_eq!(2217, perft(&board, 6, true));
    }

    #[test]
    fn perft_test_position_12() {
        let board = Board::from_fen("8/8/2k5/5q2/5n2/8/5K2/8 b - -");
        assert_eq!(23527, perft(&board, 4, true));
    }

    #[test]
    fn perft_initial_position() {
        let board = Board::initial_position();
        assert_eq!(1, perft(&board, 0, false));
        assert_eq!(20, perft(&board, 1, false));
        assert_eq!(400, perft(&board, 2, false));
        assert_eq!(8902, perft(&board, 3, false));
        assert_eq!(197281, perft(&board, 4, false));
    }

    #[test]
    fn negamax_mate_in_one() {
        let board = Board::from_fen("r3k2r/Rb6/8/8/8/2b1K3/2q4B/7R b kq - 7 4");
        let (score, moves) = negamax(&board, 2, true, &[None]);
        assert_eq!(MATE_SCORE - 1, score);
        assert_eq!("c2d2", format!("{}", moves.at(0)));
    }

    #[test]
    fn negamax_mate_in_two() {
        let board = Board::from_fen("r3k2r/Rb6/8/8/8/2b5/2q1K2B/7R w kq - 6 4");
        let (score, moves) = negamax(&board, 3, true, &[None]);
        assert_eq!(-(MATE_SCORE - 2), score);
        assert_eq!("e2e3", format!("{}", moves.at(0)));
        assert_eq!("c2d2", format!("{}", moves.at(1)));
    }

    #[test]
    fn negamax_mate_in_three() {
        let board = Board::from_fen("r3k2r/Rb5q/8/8/8/2b5/4K2B/7R b kq - 3 2");
        let (score, moves) = negamax(&board, 4, true, &[None]);
        assert_eq!(MATE_SCORE - 3, score);
        assert_eq!("h7c2", format!("{}", moves.at(0)));
    }

    #[test]
    fn negamax_mate_in_five_with_check_extension() {
        let board = Board::from_fen("5k2/6pp/1N6/4np2/2B1n2K/1P6/P4P1P/8 b - - 0 41");
        let (score, moves) = negamax(&board, 5, true, &[None]);
        assert_eq!(MATE_SCORE - 5, score);
        assert_eq!("e5g6", format!("{}", moves.at(0)));
        assert_eq!(5, moves.num_moves);
    }

    #[test]
    fn negamax_should_not_extend_search() {
        let board = Board::from_fen("r2qkb1r/pppbpppp/3p1n2/1B6/1n6/2N1PQ2/PPPPNPPP/R1B1K2R w KQkq -");
        let (score, moves) = negamax(&board, 1, true, &[None]);
        assert_eq!(1, moves.num_moves);
        assert!(moves.moves[1].is_none());
    }

    #[test]
    fn negamax_mate_in_five_with_depth_6() {
        let board = Board::from_fen("5k2/6pp/1N6/4np2/2B1n2K/1P6/P4P1P/8 b - - 0 41");
        let (score, moves) = negamax(&board, 6, true, &[None]);
        assert_eq!(MATE_SCORE - 5, score);
        assert_eq!("e5g6", format!("{}", moves.at(0)));
        assert_eq!(5, moves.num_moves);
    }

    #[test]
    fn negamax_mate_in_six() {
        let board = Board::from_fen("8/8/2k5/8/2K5/4q3/8/8 w - -");
        let (score, moves) = negamax(&board, 7, true, &[None]);
        assert_eq!(-(MATE_SCORE - 6), score);
        assert_eq!("c4b4", format!("{}", moves.at(0)));
    }
    #[test]
    fn find_shortest_path_to_mate() {
        let mut board = Board::from_fen("8/2k5/8/K7/8/4q3/8/8 b - -");
        let (score1, moves1) = negamax(&board, 6, true, &[None]);
        assert_eq!(MATE_SCORE - 3, score1);
        assert_eq!("e3b3", format!("{}", moves1.at(0)));
        board.make_move("e3b3");

        let (score2, moves2) = negamax(&board, 6, true, &[None]);
        assert_eq!(-(MATE_SCORE - 2), score2);
        assert_eq!("a5a6", format!("{}", moves2.at(0)));
        board.make_move("a5a6");

        // Two possible moves to mate, make sure we take one of them
        let (score3, moves3) = negamax(&board, 6, true, &[None]);
        assert_eq!(MATE_SCORE - 1, score3);
        board.make_move(&format!("{}", moves3.at(0)));
    }

    #[test]
    fn avoid_stale_mate_when_winning() {
        let mut board = Board::from_fen("4k3/4p3/4PP2/8/1BP4P/1P6/P2P4/RN1K1B2 w - -");
        let (_score, moves) = negamax(&board, 2, true, &[None]);
        assert!(board.make_move(&format!("{}", moves.at(0))));
        assert!(!board.is_stale_mate());
    }
}