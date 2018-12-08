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

impl Board {
    fn initial_position() -> Board {
        #[cfg_attr(feature = "cargo-clippy", allow(clippy::large_digit_groups))]
        Board {
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
        }
    }

    fn fen(&self) -> String {
        self.unicode()
            .chunks(8)
            .map(|rank| rank.iter().map(|c| unicode_to_fen(*c)))
            .map(merge_spaces)
            .collect::<Vec<_>>()
            .join("/")
    }

    fn unicode(&self) -> [char; 64] {
        let mut chars = [' '; 64];

        add_pieces(&mut chars, self.white.pawns, '♙');
        add_pieces(&mut chars, self.white.knights, '♘');
        add_pieces(&mut chars, self.white.bishops, '♗');
        add_pieces(&mut chars, self.white.rooks, '♖');
        add_pieces(&mut chars, self.white.queens, '♕');
        add_pieces(&mut chars, self.white.king, '♔');

        add_pieces(&mut chars, self.black.pawns, '♟');
        add_pieces(&mut chars, self.black.knights, '♞');
        add_pieces(&mut chars, self.black.bishops, '♝');
        add_pieces(&mut chars, self.black.rooks, '♜');
        add_pieces(&mut chars, self.black.queens, '♛');
        add_pieces(&mut chars, self.black.king, '♚');

        chars
    }

    fn print(&self) {
        for (i, rank) in self.unicode().chunks(8).enumerate() {
            println!("{} |{}|", 8 - i, rank.iter().collect::<String>());
        }
        println!("   abcdefgh");
    }
}

fn add_pieces(chars: &mut [char; 64], pieces: BitBoard, symbol: char) {
    (0..64)
        .filter(|pos| occupied(pieces, *pos))
        .for_each(|pos| chars[pos] = symbol)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initial_board_unicode() {
        assert_eq!(
            "♜♞♝♛♚♝♞♜♟♟♟♟♟♟♟♟                                ♙♙♙♙♙♙♙♙♖♘♗♕♔♗♘♖",
            Board::initial_position().unicode().iter().collect::<String>()
        );
    }

    #[test]
    fn initial_board_fen() {
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR",
            Board::initial_position().fen()
        );
    }
}

fn main() {
    let board = Board::initial_position();
    board.print();
    println!("FEN: {}", board.fen());
    println!("Board size: {} bytes", std::mem::size_of::<Board>());
}
