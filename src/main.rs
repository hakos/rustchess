type BitBoard = u64;

fn occupied(pieces: BitBoard, position: usize) -> bool {
    pieces >> position & 1 == 1
}

struct Pieces {
    pawns: BitBoard,
    rooks: BitBoard,
    knights: BitBoard,
    bishops: BitBoard,
    queens: BitBoard,
    king: BitBoard,
}

struct Board {
    white: Pieces,
    black: Pieces,
}

const INITIAL_BOARD: Board = Board {
    white: Pieces {
        pawns:   0b00000000_11111111_00000000_00000000_00000000_00000000_00000000_00000000,
        rooks:   0b10000001_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
        knights: 0b01000010_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
        bishops: 0b00100100_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
        queens:  0b00001000_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
        king:    0b00010000_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
    },
    black: Pieces {
        pawns:   0b00000000_00000000_00000000_00000000_00000000_00000000_11111111_00000000,
        rooks:   0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_10000001,
        knights: 0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_01000010,
        bishops: 0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00100100,
        queens:  0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00001000,
        king:    0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00010000,
    }
};

fn add_pieces(chars: &mut [char; 64], pieces: BitBoard, symbol: char) {
    (0..64).filter(|pos| occupied(pieces, *pos)).for_each(|pos| {
        chars[pos] = symbol
    })
}

fn board_chars(board: Board) -> [char; 64] {
    let mut chars = [' '; 64];

    add_pieces(&mut chars, board.white.pawns, '♙');
    add_pieces(&mut chars, board.white.knights, '♘');
    add_pieces(&mut chars, board.white.bishops, '♗');
    add_pieces(&mut chars, board.white.rooks, '♖');
    add_pieces(&mut chars, board.white.queens, '♕');
    add_pieces(&mut chars, board.white.king, '♔');

    add_pieces(&mut chars, board.black.pawns, '♟');
    add_pieces(&mut chars, board.black.knights, '♞');
    add_pieces(&mut chars, board.black.bishops, '♝');
    add_pieces(&mut chars, board.black.rooks, '♜');
    add_pieces(&mut chars, board.black.queens, '♛');
    add_pieces(&mut chars, board.black.king, '♚');

    chars
}

fn print_board(board: Board) {
    for (i, row) in board_chars(board).chunks(8).enumerate() {
        println!("{} |{}|", 8 - i, row.iter().collect::<String>());
    }
    println!("   abcdefgh");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initial_board_chars() {
        assert_eq!(
            "♜♞♝♛♚♝♞♜♟♟♟♟♟♟♟♟                                ♙♙♙♙♙♙♙♙♖♘♗♕♔♗♘♖",
            board_chars(INITIAL_BOARD).iter().collect::<String>());
    }
}

fn main() {
    print_board(INITIAL_BOARD);
}
