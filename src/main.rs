mod board;
mod search;

fn main() {
    use std::io;
    use std::io::prelude::*;

    let mut board = board::Board::initial_position();
    let stdin = io::stdin();

    for line in stdin.lock().lines() {
        let command = line.unwrap();
        let mut words = command.split_whitespace().into_iter();
        match words.next() {
            Some("uci") => {
                println!("id name RustChess");
                println!("id author Håkon Sandsmark");
                println!("uciok");
            }
            Some("isready") => println!("readyok"),
            Some("position") => {
                if words.next() == Some("startpos") {
                    board = board::Board::initial_position();
                    if words.next() == Some("moves") {
                        for m in words {
                            assert!(board.make_move(m));
                        }
                    }
                }
            }
            Some("go") => {
                let mut time_left = std::time::Duration::from_secs(10);
                while let Some(part) = words.next() {
                    if (part == "wtime" && board.turn() == board::Color::White)
                        || (part == "btime" && board.turn() == board::Color::Black)
                    {
                        let millis = words.next().expect("time").parse::<u64>().expect("millis");
                        time_left = std::time::Duration::from_millis(millis);
                    }
                }
                let time_budget = time_left / 15;
                let (score, moves, depth) =
                    search::negamax_iterative_deepening(&board, time_budget);
                assert!(board.make_move(&format!("{}", moves.at(0))));
                println!("bestmove {}", moves.at(0));
            }
            Some("perft") => {
                let depth = words.next().unwrap_or("6").parse::<u32>().expect("depth");
                let start = std::time::Instant::now();
                let perft = search::perft(&board, depth, false);
                let time = start.elapsed();
                println!("perft({}) = {} took {:?}", depth, perft, time);
            }
            _ => (),
        };
    }
}
