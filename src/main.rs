type BitBoard = u64;

struct Pieces {
    pawns: BitBoard,
    rooks: BitBoard,
    knights: BitBoard,
    bishops: BitBoard,
    queens: BitBoard,
    king: BitBoard,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Color {
    White,
    Black,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Movement {
    Sliding,
    Stepping,
}

fn print_moves(moves: BitBoard) {
    let mut chars = [' '; 64];
    add_piece_symbols(&mut chars, moves, '.');
    println!("Valid moves:");
    print_unicode_board(&chars);
}

const fn bitmask(bit: u8) -> BitBoard {
    1 << bit
}

const fn north_one(pieces: BitBoard) -> BitBoard {
    pieces >> 8
}

const fn south_one(pieces: BitBoard) -> BitBoard {
    pieces << 8
}

trait BitSet {
    fn clear_bit(&mut self, bit: u8);
    fn set_bit(&mut self, bit: u8);
    fn test_bit(&self, bit: u8) -> bool;
}

impl BitSet for BitBoard {
    fn clear_bit(&mut self, bit: u8) {
        *self &= !bitmask(bit);
    }
    fn set_bit(&mut self, bit: u8) {
        *self |= bitmask(bit);
    }
    fn test_bit(&self, bit: u8) -> bool {
        *self & bitmask(bit) != 0
    }
}

impl Pieces {
    fn occupancy(&self) -> BitBoard {
        self.pawns | self.rooks | self.knights | self.bishops | self.queens | self.king
    }

    fn empty(&self) -> BitBoard {
        !self.occupancy()
    }

    fn make_move(&mut self, enemies: &Pieces, src: u8, dst: u8, color: Color) -> bool {
        if self.pawns.test_bit(src) {
            let moves = if color == Color::White {
                self.get_white_pawn_moves(src, enemies)
            } else {
                self.get_black_pawn_moves(src, enemies)
            };
            print_moves(moves);
            return try_perform_move(&mut self.pawns, moves, src, dst);
        }
        if self.bishops.test_bit(src) {
            let moves = self.get_moves(src, &BISHOP_MOVES, enemies, Movement::Sliding);
            return try_perform_move(&mut self.bishops, moves, src, dst);
        }
        if self.rooks.test_bit(src) {
            let moves = self.get_moves(src, &ROOK_MOVES, enemies, Movement::Sliding);
            return try_perform_move(&mut self.rooks, moves, src, dst);
        }
        if self.knights.test_bit(src) {
            let moves = self.get_moves(src, &KNIGHT_MOVES, enemies, Movement::Stepping);
            return try_perform_move(&mut self.knights, moves, src, dst);
        }
        if self.queens.test_bit(src) {
            let moves = self.get_moves(src, &KING_QUEEN_MOVES, enemies, Movement::Sliding);
            return try_perform_move(&mut self.queens, moves, src, dst);
        }
        if self.king.test_bit(src) {
            let moves = self.get_moves(src, &KING_QUEEN_MOVES, enemies, Movement::Stepping);
            return try_perform_move(&mut self.king, moves, src, dst);
        }

        false
    }

    fn get_moves(
        &self,
        index: u8,
        offsets: &[Point],
        enemies: &Pieces,
        movement: Movement,
    ) -> BitBoard {
        let src = Point::from_index(index);
        let mut moves: BitBoard = 0;
        let empty = self.empty() & enemies.empty();
        for offset in offsets {
            let mut dst = &src + offset;
            while dst.inside_board() && empty.test_bit(dst.to_index()) {
                moves.set_bit(dst.to_index());
                dst = &dst + offset;
                if movement == Movement::Stepping {
                    break;
                }
            }
        }
        print_moves(moves);
        moves
    }

    fn get_white_pawn_moves(&self, src: u8, enemies: &Pieces) -> BitBoard {
        const RANK_4: u64 = 0x00_00_00_ff_00_00_00_00;
        let empty = self.empty() & enemies.empty();
        let single_pushs = north_one(bitmask(src)) & empty;
        let double_pushs = north_one(single_pushs) & empty & RANK_4;
        single_pushs | double_pushs
    }

    fn get_black_pawn_moves(&self, src: u8, enemies: &Pieces) -> BitBoard {
        const RANK_5: u64 = 0x00_00_00_00_ff_00_00_00;
        let empty = self.empty() & enemies.empty();
        let single_pushs = south_one(bitmask(src)) & empty;
        let double_pushs = south_one(single_pushs) & empty & RANK_5;
        single_pushs | double_pushs
    }
}

struct Board {
    white: Pieces,
    black: Pieces,
    turn: Color,
}

#[derive(Debug, PartialEq)]
struct Point {
    x: i8,
    y: i8,
}

impl Point {
    fn from_index(index: u8) -> Point {
        Point {
            x: index as i8 % 8,
            y: index as i8 / 8,
        }
    }

    fn to_index(&self) -> u8 {
        assert!(self.inside_board());
        self.y as u8 * 8 + self.x as u8
    }

    fn inside_board(&self) -> bool {
        self.x >= 0 && self.x < 8 && self.y >= 0 && self.y < 8
    }
}

use std::ops::Add;

impl Add for &Point {
    type Output = Point;

    fn add(self, other: &Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

const fn point(x: i8, y: i8) -> Point {
    Point { x, y }
}

const ROOK_MOVES: [Point; 4] = [point(-1, 0), point(0, 1), point(0, -1), point(1, 0)];
const BISHOP_MOVES: [Point; 4] = [point(-1, -1), point(-1, 1), point(1, -1), point(1, 1)];
const KNIGHT_MOVES: [Point; 8] = [
    point(-2, -1),
    point(-2, 1),
    point(-1, -2),
    point(-1, 2),
    point(1, -2),
    point(1, 2),
    point(2, -1),
    point(2, 1),
];
const KING_QUEEN_MOVES: [Point; 8] = [
    point(-1, -1),
    point(-1, 0),
    point(-1, 1),
    point(0, -1),
    point(0, 1),
    point(1, -1),
    point(1, 0),
    point(1, 1),
];

fn print_unicode_board(unicode: &[char]) {
    for (i, rank) in unicode.chunks(8).enumerate() {
        println!("{} |{}|", 8 - i, rank.iter().collect::<String>());
    }
    println!("   abcdefgh");
}

fn try_perform_move(pieces: &mut BitBoard, moves: BitBoard, src: u8, dst: u8) -> bool {
    if moves.test_bit(dst) {
        pieces.clear_bit(src);
        pieces.set_bit(dst);
        true
    } else {
        false
    }
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
            turn: Color::White,
        }
    }

    fn as_fen(&self) -> String {
        self.as_unicode()
            .chunks(8)
            .map(|rank| rank.iter().map(|c| unicode_to_fen(*c)))
            .map(merge_spaces)
            .collect::<Vec<_>>()
            .join("/")
    }

    fn as_unicode(&self) -> [char; 64] {
        let mut chars = [' '; 64];

        add_piece_symbols(&mut chars, self.white.pawns, '♙');
        add_piece_symbols(&mut chars, self.white.knights, '♘');
        add_piece_symbols(&mut chars, self.white.bishops, '♗');
        add_piece_symbols(&mut chars, self.white.rooks, '♖');
        add_piece_symbols(&mut chars, self.white.queens, '♕');
        add_piece_symbols(&mut chars, self.white.king, '♔');

        add_piece_symbols(&mut chars, self.black.pawns, '♟');
        add_piece_symbols(&mut chars, self.black.knights, '♞');
        add_piece_symbols(&mut chars, self.black.bishops, '♝');
        add_piece_symbols(&mut chars, self.black.rooks, '♜');
        add_piece_symbols(&mut chars, self.black.queens, '♛');
        add_piece_symbols(&mut chars, self.black.king, '♚');

        chars
    }

    fn print(&self) {
        print_unicode_board(&self.as_unicode());
    }

    fn make_move(&mut self, m: &str) -> bool {
        if m.len() != 4 {
            return false;
        }
        let src = match str_to_index(&m[0..2]) {
            Some(src) => src,
            None => return false,
        };
        let dst = match str_to_index(&m[2..4]) {
            Some(dst) => dst,
            None => return false,
        };

        if self.turn == Color::White {
            if self.white.make_move(&self.black, src, dst, self.turn) {
                self.turn = Color::Black;
                return true;
            }
        } else if self.black.make_move(&self.white, src, dst, self.turn) {
            self.turn = Color::White;
            return true;
        }

        false
    }
}

fn str_to_index(s: &str) -> Option<u8> {
    assert_eq!(2, s.len());
    // file: a-h
    let file = match u8::from_str_radix(&s[0..1], 18) {
        Ok(n) => {
            if n >= 10 && n <= 17 {
                n - 10
            } else {
                return None;
            }
        }
        Err(_) => return None,
    };
    // rank: 1-8
    let rank = match u8::from_str_radix(&s[1..2], 10) {
        Ok(n) => {
            if n >= 1 && n <= 8 {
                n - 1
            } else {
                return None;
            }
        }
        Err(_) => return None,
    };
    Some(63 - 8 * rank - (7 - file))
}

fn add_piece_symbols(chars: &mut [char; 64], pieces: BitBoard, symbol: char) {
    (0..64)
        .filter(|index| pieces.test_bit(*index as u8))
        .for_each(|index| chars[index] = symbol)
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
            Board::initial_position().as_unicode().iter().collect::<String>()
        );
    }

    #[test]
    fn initial_board_fen() {
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR",
            Board::initial_position().as_fen()
        );
    }

    #[test]
    fn make_illegal_moves() {
        let mut board = Board::initial_position();
        assert!(!board.make_move("0000"));
        assert!(!board.make_move("a0a0"));
        assert!(!board.make_move("e2"));
        assert!(!board.make_move("e3e4")); // invalid source
    }

    #[test]
    fn move_pawns() {
        let mut board = Board::initial_position();
        assert!(!board.make_move("e2e5")); // too far
        assert!(board.make_move("e2e4"));
        assert!(board.make_move("e7e5"));
        assert!(!board.make_move("e4e5")); // occupied
        assert_eq!(
            "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR",
            board.as_fen()
        );
    }

    #[test]
    fn move_correct_pawn() {
        let mut board = Board::initial_position();
        assert!(!board.make_move("a2b4")); // too far
    }

    #[test]
    fn move_bishop() {
        let mut board = Board::initial_position();
        assert!(board.make_move("d2d4"));
        assert!(!board.make_move("c1c2"));
        board.turn = Color::White;
        assert!(board.make_move("c1d2"));
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/3P4/8/PPPBPPPP/RN1QKBNR",
            board.as_fen()
        );
    }

    #[test]
    fn move_rook() {
        let mut board = Board::initial_position();
        assert!(!board.make_move("a1a2"));
        assert!(!board.make_move("a1b1"));
        assert!(board.make_move("a2a4"));
        board.turn = Color::White;
        assert!(board.make_move("a1a2"));
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/P7/8/RPPPPPPP/1NBQKBNR",
            board.as_fen()
        );
    }

    #[test]
    fn move_knight() {
        let mut board = Board::initial_position();
        assert!(board.make_move("b1c3"));
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/R1BQKBNR",
            board.as_fen()
        );
        assert_eq!(Color::Black, board.turn);
        assert!(board.make_move("b8c6"));
    }

    #[test]
    fn move_king() {
        let mut board = Board::initial_position();
        assert!(board.make_move("e2e4"));
        board.turn = Color::White;
        assert!(!board.make_move("e1e3")); // too far
        assert!(board.make_move("e1e2"));
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPPKPPP/RNBQ1BNR",
            board.as_fen()
        );
    }

    #[test]
    fn move_queen() {
        let mut board = Board::initial_position();
        assert!(board.make_move("d2d4"));
        board.turn = Color::White;
        assert!(board.make_move("d1d3"));
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/3P4/3Q4/PPP1PPPP/RNB1KBNR",
            board.as_fen()
        );
    }

    #[test]
    fn test_str_to_index() {
        assert!(str_to_index("a0").is_none());
        assert!(str_to_index("a9").is_none());
        assert!(str_to_index("i1").is_none());
        assert!(str_to_index("91").is_none());

        assert_eq!(0, str_to_index("a8").unwrap());
        assert_eq!(7, str_to_index("h8").unwrap());
        assert_eq!(56, str_to_index("a1").unwrap());
        assert_eq!(63, str_to_index("h1").unwrap());
    }
}

fn main() {
    use std::io;
    use std::io::prelude::*;

    println!("Welcome to Rust Chess!");

    let mut board = Board::initial_position();

    let print_state = |board: &Board| {
        println!("FEN: {}", board.as_fen());
        board.print();
        println!("{:?}'s turn", board.turn);
    };

    print_state(&board);
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let user_move = line.unwrap();
        if board.make_move(&user_move) {
            print_state(&board);
        } else {
            println!("Illegal move, try again")
        }
    }
}
