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

#[cfg_attr(feature = "cargo-clippy", allow(clippy::large_digit_groups))]
const INITIAL_BOARD: Board = Board {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    white: Pieces {
        pawns:   0b00000000_11111111_00000000_00000000_00000000_00000000_00000000_00000000,
        rooks:   0b10000001_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
        knights: 0b01000010_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
        bishops: 0b00100100_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
        queens:  0b00001000_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
        king:    0b00010000_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
    },
    #[cfg_attr(rustfmt, rustfmt_skip)]
    black: Pieces {
        pawns:   0b00000000_00000000_00000000_00000000_00000000_00000000_11111111_00000000,
        rooks:   0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_10000001,
        knights: 0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_01000010,
        bishops: 0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00100100,
        queens:  0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00001000,
        king:    0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00010000,
    },
};

fn add_pieces(chars: &mut [char; 64], pieces: BitBoard, symbol: char) {
    (0..64)
        .filter(|pos| occupied(pieces, *pos))
        .for_each(|pos| chars[pos] = symbol)
}

fn board_unicode(board: &Board) -> [char; 64] {
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

fn unicode_to_fen(c: char) -> char {
    match c {
        '♙' => 'P',
        '♘' => 'N',
        '♗' => 'B',
        '♖' => 'R',
        '♕' => 'Q',
        '♔' => 'K',
        '♟' => 'p',
        '♞' => 'n',
        '♝' => 'b',
        '♜' => 'r',
        '♛' => 'q',
        '♚' => 'k',
        ' ' => ' ',
        _ => panic!("Unknown char '{}'", c),
    }
}

fn merge_spaces(chars: impl Iterator<Item = char>) -> String {
    use itertools::Itertools; // coalesce
    chars
        .map(|c| if c == ' ' { '1' } else { c })
        .coalesce(|c, d| {
            if c.is_digit(10) && d == '1' {
                let count = c.to_digit(10).unwrap() + 1;
                Ok(std::char::from_digit(count, 10).unwrap())
            } else {
                Err((c, d))
            }
        })
        .collect()
}

fn board_fen(board: &Board) -> String {
    board_unicode(board)
        .chunks(8)
        .map(|rank| rank.iter().map(|c| unicode_to_fen(*c)))
        .map(merge_spaces)
        .collect::<Vec<_>>()
        .join("/")
}

fn print_board(board: &Board) {
    for (i, rank) in board_unicode(board).chunks(8).enumerate() {
        println!("{} |{}|", 8 - i, rank.iter().collect::<String>());
    }
    println!("   abcdefgh");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initial_board_unicode() {
        assert_eq!(
            "♜♞♝♛♚♝♞♜♟♟♟♟♟♟♟♟                                ♙♙♙♙♙♙♙♙♖♘♗♕♔♗♘♖",
            board_unicode(&INITIAL_BOARD).iter().collect::<String>()
        );
    }

    #[test]
    fn initial_board_fen() {
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR",
            board_fen(&INITIAL_BOARD)
        );
    }
}

fn main() {
    print_board(&INITIAL_BOARD);
    println!("FEN: {}", board_fen(&INITIAL_BOARD));
}
