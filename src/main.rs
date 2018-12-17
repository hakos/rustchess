mod board;

fn main() {
    use std::io;
    use std::io::prelude::*;

    let mut board = board::Board::initial_position();
    let stdin = io::stdin();

    for line in stdin.lock().lines() {
        let command = line.unwrap();
        let mut parts = command.split_whitespace().into_iter();
        match parts.next() {
            Some("uci") => {
                println!("id name RustChess");
                println!("id author Håkon Sandsmark");
                println!("uciok");
            },
            Some("isready") => println!("readyok"),
            Some("position") => {
                if parts.next() == Some("startpos") && parts.next() == Some("moves") {
                    board = board::Board::initial_position();
                    for m in parts {
                        assert!(board.make_move(m));
                    }
                }
            },
            Some("go") => {
                let mut time_left = std::time::Duration::from_secs(10);
                while let Some(part) = parts.next() {
                    if (part == "wtime" && board.turn() == board::Color::White)
                    || (part == "btime" && board.turn() == board::Color::Black) {
                        let millis = parts.next().expect("time").parse::<u64>().unwrap();
                        time_left = std::time::Duration::from_millis(millis);
                    }
                }
                let time_budget = time_left / 10;
                println!("info string search budget {:?}", time_budget);
                let computer_move = board.negamax_iterative_deepening(time_budget);
                assert!(board.make_move(&format!("{}", computer_move.1)));
                println!("info string moved {} with evaluation {}",
                    computer_move.1, computer_move.0 as f32 / 10.0);
                println!("bestmove {}", computer_move.1);
            }
            _ => (),
        };
    }
}
