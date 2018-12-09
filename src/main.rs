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
    fn cleared() -> Pieces {
        Pieces {
            pawns: 0,
            rooks: 0,
            knights: 0,
            bishops: 0,
            queens: 0,
            king: 0,
        }
    }

    fn occupancy(&self) -> BitBoard {
        self.pawns | self.rooks | self.knights | self.bishops | self.queens | self.king
    }

    fn empty(&self) -> BitBoard {
        !self.occupancy()
    }

    fn piece_count(&self) -> u32 {
        self.occupancy().count_ones()
    }

    fn capture(&mut self, index: u8) {
        self.pawns.clear_bit(index);
        self.rooks.clear_bit(index);
        self.knights.clear_bit(index);
        self.bishops.clear_bit(index);
        self.queens.clear_bit(index);
        self.king.clear_bit(index);
    }

    fn make_move(&mut self, enemies: &mut Pieces, src: u8, dst: u8, color: Color) -> bool {
        if self.pawns.test_bit(src) {
            let moves = if color == Color::White {
                self.get_white_pawn_moves(src, enemies)
            } else {
                self.get_black_pawn_moves(src, enemies)
            };
            print_moves(moves);
            apply_move(&mut self.pawns, moves, src, dst, enemies)
        } else if self.bishops.test_bit(src) {
            let moves = self.get_moves(src, &BISHOP_MOVES, enemies, Movement::Sliding);
            apply_move(&mut self.bishops, moves, src, dst, enemies)
        } else if self.rooks.test_bit(src) {
            let moves = self.get_moves(src, &ROOK_MOVES, enemies, Movement::Sliding);
            apply_move(&mut self.rooks, moves, src, dst, enemies)
        } else if self.knights.test_bit(src) {
            let moves = self.get_moves(src, &KNIGHT_MOVES, enemies, Movement::Stepping);
            apply_move(&mut self.knights, moves, src, dst, enemies)
        } else if self.queens.test_bit(src) {
            let moves = self.get_moves(src, &KING_QUEEN_MOVES, enemies, Movement::Sliding);
            apply_move(&mut self.queens, moves, src, dst, enemies)
        } else if self.king.test_bit(src) {
            let moves = self.get_moves(src, &KING_QUEEN_MOVES, enemies, Movement::Stepping);
            apply_move(&mut self.king, moves, src, dst, enemies)
        } else {
            false
        }
    }

    fn get_moves(
        &self,
        index: u8,
        directions: &[Point],
        enemies: &Pieces,
        movement: Movement,
    ) -> BitBoard {
        let src = Point::from_index(index);
        let friendly_pieces = self.occupancy();
        let enemy_pieces = enemies.occupancy();
        let mut moves: BitBoard = 0;
        for direction in directions {
            let mut dst = &src + direction;
            while dst.inside_board() && !friendly_pieces.test_bit(dst.to_index()) {
                let dst_index = dst.to_index();
                moves.set_bit(dst_index);
                let capture = enemy_pieces.test_bit(dst_index);
                if movement == Movement::Stepping || capture {
                    // Stop tracing this direction
                    break;
                }
                dst = &dst + direction;
            }
        }
        print_moves(moves);
        moves
    }

    fn get_white_pawn_moves(&self, src: u8, enemies: &Pieces) -> BitBoard {
        const RANK_4: u64 = 0x00_00_00_ff_00_00_00_00;
        let empty = self.empty() & enemies.empty();
        let single_push = north_one(bitmask(src)) & empty;
        let double_push = north_one(single_push) & empty & RANK_4;
        let captures = self.get_moves(src, &WHITE_PAWN_CAPTURES, enemies, Movement::Stepping)
            & enemies.occupancy();
        single_push | double_push | captures
    }

    fn get_black_pawn_moves(&self, src: u8, enemies: &Pieces) -> BitBoard {
        const RANK_5: u64 = 0x00_00_00_00_ff_00_00_00;
        let empty = self.empty() & enemies.empty();
        let single_push = south_one(bitmask(src)) & empty;
        let double_push = south_one(single_push) & empty & RANK_5;
        let captures = self.get_moves(src, &BLACK_PAWN_CAPTURES, enemies, Movement::Stepping)
            & enemies.occupancy();
        single_push | double_push | captures
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
const WHITE_PAWN_CAPTURES: [Point; 2] = [point(-1, -1), point(1, -1)];
const BLACK_PAWN_CAPTURES: [Point; 2] = [point(-1, 1), point(1, 1)];

fn print_unicode_board(unicode: &[char]) {
    for (i, rank) in unicode.chunks(8).enumerate() {
        println!("{} |{}|", 8 - i, rank.iter().collect::<String>());
    }
    println!("   abcdefgh");
}

fn apply_move(
    pieces: &mut BitBoard,
    allowed_moves: BitBoard,
    src: u8,
    dst: u8,
    enemies: &mut Pieces,
) -> bool {
    if allowed_moves.test_bit(dst) {
        pieces.clear_bit(src);
        pieces.set_bit(dst);
        enemies.capture(dst);
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

    fn cleared() -> Board {
        Board {
            white: Pieces::cleared(),
            black: Pieces::cleared(),
            turn: Color::White,
        }
    }

    fn from_fen(fen: &str) -> Board {
        let mut board = Board::cleared();
        let mut index = 0;
        for c in fen.chars() {
            let piece = match c {
                'r' => Some(&mut board.black.rooks),
                'n' => Some(&mut board.black.knights),
                'b' => Some(&mut board.black.bishops),
                'q' => Some(&mut board.black.queens),
                'k' => Some(&mut board.black.king),
                'p' => Some(&mut board.black.pawns),
                'R' => Some(&mut board.white.rooks),
                'N' => Some(&mut board.white.knights),
                'B' => Some(&mut board.white.bishops),
                'Q' => Some(&mut board.white.queens),
                'K' => Some(&mut board.white.king),
                'P' => Some(&mut board.white.pawns),
                '/' => {
                    assert!(index % 8 == 0);
                    None
                }
                _ => {
                    index += c.to_digit(10).expect("Expected digit");
                    None
                }
            };
            if piece.is_some() {
                piece.unwrap().set_bit(index as u8);
                index += 1;
            };
        }
        assert_eq!(64, index);
        board
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
        self.check_invariants();

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
            if self.white.make_move(&mut self.black, src, dst, self.turn) {
                self.turn = Color::Black;
                return true;
            }
        } else if self.black.make_move(&mut self.white, src, dst, self.turn) {
            self.turn = Color::White;
            return true;
        }

        false
    }

    fn check_invariants(&self) {
        // No overlapping pieces
        assert_eq!(0, self.white.occupancy() & self.black.occupancy());
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
    fn parse_fen() {
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR",
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR").as_fen(),
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
    fn pawn_movement() {
        let mut board = Board::initial_position();
        assert!(!board.make_move("a2b4"));
        assert!(board.make_move("d2d4"));
        assert!(board.make_move("d7d5"));
        assert!(!board.make_move("d4e5")); // no capture
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
    fn capture_bishop() {
        let mut board = Board::from_fen("rn1qkbnr/ppp1pppp/8/3p4/4P1b1/8/PPPPBPPP/RNBQK1NR");
        assert!(!board.make_move("e2h5")); // past enemy
        assert_eq!(16, board.white.piece_count());
        assert_eq!(16, board.black.piece_count());
        assert!(board.make_move("e2g4")); // capture
        assert_eq!(16, board.white.piece_count());
        assert_eq!(15, board.black.piece_count());
    }

    #[test]
    fn white_pawn_capture() {
        let mut board = Board::from_fen("rnbqkbnr/pppp1ppp/8/4p3/3P4/8/PPP1PPPP/RNBQKBNR");
        assert_eq!(8, board.black.pawns.count_ones());
        assert!(board.make_move("d4e5")); // capture
        assert_eq!(7, board.black.pawns.count_ones());
    }

    #[test]
    fn black_pawn_capture() {
        let mut board = Board::from_fen("rnbqkbnr/pppp1ppp/8/4p3/3P4/8/PPP1PPPP/RNBQKBNR");
        board.turn = Color::Black;
        assert_eq!(8, board.white.pawns.count_ones());
        assert!(board.make_move("e5d4")); // capture
        assert_eq!(7, board.white.pawns.count_ones());
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
