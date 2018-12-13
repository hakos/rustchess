mod board;

fn main() {
    use std::env;
    use std::io;
    use std::io::prelude::*;

    println!("Welcome to Rust Chess!");

    let mut board = if let Some(fen) = env::args().nth(1) {
        board::Board::from_fen(&fen)
    } else {
        board::Board::initial_position()
    };

    let print_state = |board: &board::Board| {
        println!("FEN: {}", board.as_fen());
        board.print();
        if board.is_check_mated() {
            println!("Check mate! {:?} wins", board.turn().other());
            std::process::exit(0);
        };
        if board.is_checked() {
            print!("Check! ");
        };
        println!("{:?}'s turn", board.turn());
    };

    print_state(&board);
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let user_move = line.unwrap();
        if board.make_move(&user_move) {
            print_state(&board);
        } else {
            println!("Illegal move, try again");
        }
    }
    println!("Bye");
}
