type BitBoard = u64;

#[derive(Copy, Clone)]
struct Pieces {
    pawns: BitBoard,
    rooks: BitBoard,
    knights: BitBoard,
    bishops: BitBoard,
    queens: BitBoard,
    king: BitBoard,
    can_king_side_castle: bool,
    can_queen_side_castle: bool,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Color {
    White,
    Black,
}

impl Color {
    fn other(self) -> Color {
        if self == Color::White {
            Color::Black
        } else {
            Color::White
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Movement {
    Sliding,
    Stepping,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Promotion {
    Rook,
    Knight,
    Bishop,
    Queen,
}

#[allow(dead_code)]
fn print_bits(moves: BitBoard) {
    let mut chars = ['.'; 64];
    add_piece_symbols(&mut chars, moves, '1');
    print_unicode_board(&chars);
}

const fn bitmask(bit: u8) -> BitBoard {
    1 << bit
}

const fn shift_north(pieces: BitBoard) -> BitBoard {
    pieces >> 8
}

const fn shift_south(pieces: BitBoard) -> BitBoard {
    pieces << 8
}

trait BitSet {
    fn clear_bit(&mut self, bit: u8);
    fn set_bit(&mut self, bit: u8);
    fn test_bit(&self, bit: u8) -> bool;
    fn count(&self) -> u32;
    fn first_bit(&self) -> u8;
    fn for_each_bit<F>(&self, f: F)
    where
        F: FnMut(u8);
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
    fn count(&self) -> u32 {
        self.count_ones()
    }
    fn first_bit(&self) -> u8 {
        self.trailing_zeros() as u8
    }
    fn for_each_bit<F>(&self, mut f: F)
    where
        F: FnMut(u8),
    {
        let mut copy = *self;
        while copy != 0 {
            let next_bit = copy.first_bit();
            f(next_bit);
            copy.clear_bit(next_bit);
        }
    }
}

fn is_promotion_rank(dst: u8, color: Color) -> bool {
    const RANK_1: u64 = 0xff_00_00_00_00_00_00_00;
    const RANK_8: u64 = 0x00_00_00_00_00_00_00_ff;
    let promotion_rank = if color == Color::White {
        RANK_8
    } else {
        RANK_1
    };
    promotion_rank.test_bit(dst)
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
            can_king_side_castle: true,
            can_queen_side_castle: true,
        }
    }

    fn occupancy(&self) -> BitBoard {
        self.pawns | self.rooks | self.knights | self.bishops | self.queens | self.king
    }

    fn empty(&self) -> BitBoard {
        !self.occupancy()
    }

    fn capture(&mut self, index: u8) {
        self.pawns.clear_bit(index);
        self.rooks.clear_bit(index);
        self.knights.clear_bit(index);
        self.bishops.clear_bit(index);
        self.queens.clear_bit(index);
        self.king.clear_bit(index);
    }

    fn apply_move_impl(
        &mut self,
        enemies: &mut Pieces,
        src: u8,
        dst: u8,
        promotion: Option<Promotion>,
        color: Color,
    ) -> bool {
        if self.pawns.test_bit(src) {
            if is_promotion_rank(dst, color) {
                match promotion {
                    Some(Promotion::Bishop) => self.bishops.set_bit(dst),
                    Some(Promotion::Rook) => self.rooks.set_bit(dst),
                    Some(Promotion::Knight) => self.knights.set_bit(dst),
                    Some(Promotion::Queen) => self.queens.set_bit(dst),
                    None => return false,
                }
            } else {
                match promotion {
                    Some(_) => return false,
                    None => self.pawns.set_bit(dst),
                }
            }
            self.pawns.clear_bit(src);
            enemies.capture(dst);
        } else if self.bishops.test_bit(src) {
            apply_move(&mut self.bishops, src, dst, enemies);
        } else if self.rooks.test_bit(src) {
            apply_move(&mut self.rooks, src, dst, enemies);
        } else if self.knights.test_bit(src) {
            apply_move(&mut self.knights, src, dst, enemies);
        } else if self.queens.test_bit(src) {
            apply_move(&mut self.queens, src, dst, enemies);
        } else if self.king.test_bit(src) {
            apply_move(&mut self.king, src, dst, enemies);
        } else {
            panic!("Unknown piece!");
        }

        true
    }

    fn make_move(
        &mut self,
        enemies: &mut Pieces,
        src: u8,
        dst: u8,
        promotion: Option<Promotion>,
        color: Color,
    ) -> bool {
        let allowed_moves = self.get_moves(enemies, color)[src as usize];
        if allowed_moves.test_bit(dst) {
            self.apply_move_impl(enemies, src, dst, promotion, color)
        } else {
            false
        }
    }

    fn get_moves_from_square(
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
        moves
    }

    fn get_moves(&self, enemies: &Pieces, color: Color) -> [BitBoard; 64] {
        let mut moves: [BitBoard; 64] = [0; 64];

        self.pawns.for_each_bit(|src| {
            moves[src as usize] = if color == Color::White {
                self.get_white_pawn_moves(src, enemies)
            } else {
                self.get_black_pawn_moves(src, enemies)
            };
        });

        self.bishops.for_each_bit(|src| {
            moves[src as usize] =
                self.get_moves_from_square(src, &BISHOP_MOVES, enemies, Movement::Sliding);
        });

        self.rooks.for_each_bit(|src| {
            moves[src as usize] =
                self.get_moves_from_square(src, &ROOK_MOVES, enemies, Movement::Sliding);
        });

        self.knights.for_each_bit(|src| {
            moves[src as usize] =
                self.get_moves_from_square(src, &KNIGHT_MOVES, enemies, Movement::Stepping);
        });

        self.queens.for_each_bit(|src| {
            moves[src as usize] =
                self.get_moves_from_square(src, &KING_QUEEN_MOVES, enemies, Movement::Sliding);
        });

        assert_eq!(1, self.king.count());
        let king_index = self.king.first_bit();
        moves[king_index as usize] =
            self.get_moves_from_square(king_index, &KING_QUEEN_MOVES, enemies, Movement::Stepping);

        self.remove_moves_that_leave_us_checked(&mut moves, enemies, color);

        moves
    }

    #[cfg(test)]
    fn count_moves(&self, enemies: &Pieces, color: Color) -> u32 {
        let moves = self.get_moves(enemies, color);
        let num_moves: u32 = moves.iter().map(|dsts| dsts.count()).sum();

        let moves_from_promotion_squares = if color == Color::White {
            moves.iter().enumerate().skip(8).take(8)
        } else {
            moves.iter().enumerate().skip(48).take(8)
        };

        let num_promotions: u32 = moves_from_promotion_squares
            .filter(|(src, _dsts)| self.pawns.test_bit(*src as u8))
            .map(|(_src, dsts)| dsts.count())
            .sum();

        num_moves + 3 * num_promotions
    }

    fn remove_moves_that_leave_us_checked(
        &self,
        moves: &mut [BitBoard; 64],
        enemies: &Pieces,
        color: Color,
    ) {
        for (src, dsts) in moves.iter_mut().enumerate() {
            let dsts_copy = *dsts;
            dsts_copy.for_each_bit(|dst| {
                let mut enemies_copy = *enemies;
                let mut self_copy = *self;
                self_copy.apply_move_impl(&mut enemies_copy, src as u8, dst, None, color);
                if self_copy.is_checked_by(&enemies_copy, color.other()) {
                    dsts.clear_bit(dst);
                }
            });
        }
    }

    fn get_white_pawn_moves(&self, src: u8, enemies: &Pieces) -> BitBoard {
        const RANK_4: u64 = 0x00_00_00_ff_00_00_00_00;
        let empty = self.empty() & enemies.empty();
        let single_push = shift_north(bitmask(src)) & empty;
        let double_push = shift_north(single_push) & empty & RANK_4;
        let captures =
            self.get_moves_from_square(src, &WHITE_PAWN_CAPTURES, enemies, Movement::Stepping)
                & enemies.occupancy();
        single_push | double_push | captures
    }

    fn get_black_pawn_moves(&self, src: u8, enemies: &Pieces) -> BitBoard {
        const RANK_5: u64 = 0x00_00_00_00_ff_00_00_00;
        let empty = self.empty() & enemies.empty();
        let single_push = shift_south(bitmask(src)) & empty;
        let double_push = shift_south(single_push) & empty & RANK_5;
        let captures =
            self.get_moves_from_square(src, &BLACK_PAWN_CAPTURES, enemies, Movement::Stepping)
                & enemies.occupancy();
        single_push | double_push | captures
    }

    fn square_is_attacked_by(&self, index: u8, enemies: &Pieces, attacker: Color) -> bool {
        let rook_attack_sources =
            self.get_moves_from_square(index, &ROOK_MOVES, enemies, Movement::Sliding);
        let enemy_rooks_and_queens = enemies.rooks | enemies.queens;
        if rook_attack_sources & enemy_rooks_and_queens != 0 {
            return true;
        }

        let bishop_attack_sources =
            self.get_moves_from_square(index, &BISHOP_MOVES, enemies, Movement::Sliding);
        let enemy_bishops_and_queens = enemies.bishops | enemies.queens;
        if bishop_attack_sources & enemy_bishops_and_queens != 0 {
            return true;
        }

        let knight_attack_sources =
            self.get_moves_from_square(index, &KNIGHT_MOVES, enemies, Movement::Stepping);
        if knight_attack_sources & enemies.knights != 0 {
            return true;
        }

        let king_attack_sources =
            self.get_moves_from_square(index, &KING_QUEEN_MOVES, enemies, Movement::Stepping);
        if king_attack_sources & enemies.king != 0 {
            return true;
        }

        let pawn_captures = if attacker == Color::White {
            &BLACK_PAWN_CAPTURES
        } else {
            &WHITE_PAWN_CAPTURES
        };
        let pawn_attack_sources =
            self.get_moves_from_square(index, pawn_captures, enemies, Movement::Stepping);
        if pawn_attack_sources & enemies.pawns != 0 {
            return true;
        }

        false
    }

    fn is_checked_by(&self, enemies: &Pieces, attacker: Color) -> bool {
        assert_eq!(1, self.king.count());
        let king_index = self.king.first_bit();
        self.square_is_attacked_by(king_index, enemies, attacker)
    }
}

#[derive(Copy, Clone)]
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

fn apply_move(pieces: &mut BitBoard, src: u8, dst: u8, enemies: &mut Pieces) {
    pieces.clear_bit(src);
    pieces.set_bit(dst);
    enemies.capture(dst);
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
                can_king_side_castle: true,
                can_queen_side_castle: true,
            },
            #[cfg_attr(rustfmt, rustfmt_skip)]
            black: Pieces {
                pawns:   0b00000000_00000000_00000000_00000000_00000000_00000000_11111111_00000000,
                rooks:   0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_10000001,
                knights: 0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_01000010,
                bishops: 0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00100100,
                queens:  0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00001000,
                king:    0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00010000,
                can_king_side_castle: true,
                can_queen_side_castle: true,
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
        let mut fen_parts = fen.split(" ");

        let mut index = 0;
        for c in fen_parts.next().unwrap().chars() {
            if let Some(piece) = match c {
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
                    index += c
                        .to_digit(9)
                        .unwrap_or_else(|| panic!("Unknown char '{}' in FEN string", c));
                    None
                }
            } {
                piece.set_bit(index as u8);
                index += 1;
            };
        }
        assert_eq!(64, index);

        match fen_parts.next() {
            Some("w") => board.turn = Color::White,
            Some("b") => board.turn = Color::Black,
            Some(turn) => panic!("Unknown turn identifier '{}' in FEN string", turn),
            None => (),
        }

        if let Some(castling) = fen_parts.next() {
            board.white.can_king_side_castle = false;
            board.white.can_queen_side_castle = false;
            board.black.can_king_side_castle = false;
            board.black.can_queen_side_castle = false;
            for c in castling.chars() {
                match c {
                    'K' => board.white.can_king_side_castle = true,
                    'Q' => board.white.can_queen_side_castle = true,
                    'k' => board.black.can_king_side_castle = true,
                    'q' => board.black.can_queen_side_castle = true,
                    '-' => (),
                    _ => panic!("Unknown castling identifier '{}' in FEN string", c),
                }
            }
        }

        board
    }

    fn as_fen(&self) -> String {
        let position = self.as_unicode()
            .chunks(8)
            .map(|rank| rank.iter().map(|c| unicode_to_fen(*c)))
            .map(merge_spaces)
            .collect::<Vec<_>>()
            .join("/");

        let turn = match self.turn {
            Color::White => 'w',
            Color::Black => 'b',
        };

        let mut castling = String::new();
        if self.white.can_king_side_castle {
            castling.push('K');
        }
        if self.white.can_queen_side_castle {
            castling.push('Q');
        }
        if self.black.can_king_side_castle {
            castling.push('k');
        }
        if self.black.can_queen_side_castle {
            castling.push('q');
        }
        if castling.is_empty() {
            castling.push('-');
        }

        format!("{} {} {}", position, turn, castling)
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
        if m.len() < 4 || m.len() > 5 {
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
        let promotion = if let Some(c) = m.chars().nth(4) {
            match c {
                'q' => Some(Promotion::Queen),
                'r' => Some(Promotion::Rook),
                'n' => Some(Promotion::Knight),
                'b' => Some(Promotion::Bishop),
                _ => return false,
            }
        } else {
            None
        };

        // Copy to be able to restore if move ends up in check
        let self_before_move = *self;

        if self.turn == Color::White {
            if !self
                .white
                .make_move(&mut self.black, src, dst, promotion, self.turn)
            {
                return false;
            }
        } else if !self
            .black
            .make_move(&mut self.white, src, dst, promotion, self.turn)
        {
            return false;
        }

        if self.is_checked(self.turn) {
            // Restore board
            *self = self_before_move;
            return false;
        }

        self.turn = self.turn.other();

        true
    }

    #[cfg(test)]
    fn square_is_attacked_by(&self, index: u8, attacker: Color) -> bool {
        if attacker == Color::White {
            self.black
                .square_is_attacked_by(index, &self.white, attacker)
        } else {
            self.white
                .square_is_attacked_by(index, &self.black, attacker)
        }
    }

    fn is_checked(&self, color: Color) -> bool {
        if color == Color::White {
            self.white.is_checked_by(&self.black, Color::Black)
        } else {
            self.black.is_checked_by(&self.white, Color::White)
        }
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
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq",
            Board::initial_position().as_fen()
        );
    }

    #[test]
    fn parse_fen() {
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b Kk",
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b Kk").as_fen(),
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
            "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq",
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
            "rnbqkbnr/pppppppp/8/8/3P4/8/PPPBPPPP/RN1QKBNR b KQkq",
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
            "rnbqkbnr/pppppppp/8/8/P7/8/RPPPPPPP/1NBQKBNR b KQkq",
            board.as_fen()
        );
    }

    #[test]
    fn move_knight() {
        let mut board = Board::initial_position();
        assert!(board.make_move("b1c3"));
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/R1BQKBNR b KQkq",
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
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPPKPPP/RNBQ1BNR b KQkq",
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
            "rnbqkbnr/pppppppp/8/8/3P4/3Q4/PPP1PPPP/RNB1KBNR b KQkq",
            board.as_fen()
        );
    }

    #[test]
    fn capture_bishop() {
        let mut board = Board::from_fen("rn1qkbnr/ppp1pppp/8/3p4/4P1b1/8/PPPPBPPP/RNBQK1NR");
        assert!(!board.make_move("e2h5")); // past enemy
        assert_eq!(2, board.black.bishops.count());
        assert!(board.make_move("e2g4")); // capture
        assert_eq!(1, board.black.bishops.count());
    }

    #[test]
    fn white_pawn_capture() {
        let mut board = Board::from_fen("rnbqkbnr/pppp1ppp/8/4p3/3P4/8/PPP1PPPP/RNBQKBNR");
        assert_eq!(8, board.black.pawns.count());
        assert!(board.make_move("d4e5")); // capture
        assert_eq!(7, board.black.pawns.count());
    }

    #[test]
    fn black_pawn_capture() {
        let mut board = Board::from_fen("rnbqkbnr/pppp1ppp/8/4p3/3P4/8/PPP1PPPP/RNBQKBNR");
        board.turn = Color::Black;
        assert_eq!(8, board.white.pawns.count());
        assert!(board.make_move("e5d4")); // capture
        assert_eq!(7, board.white.pawns.count());
    }

    #[test]
    fn square_is_attacked_by_rook() {
        let mut board = Board::cleared();
        board.white.rooks.set_bit(str_to_index("a1").unwrap());
        assert!(!board.square_is_attacked_by(str_to_index("a1").unwrap(), Color::White));
        assert!(board.square_is_attacked_by(str_to_index("b1").unwrap(), Color::White));
        assert!(board.square_is_attacked_by(str_to_index("a2").unwrap(), Color::White));
        assert!(!board.square_is_attacked_by(str_to_index("b2").unwrap(), Color::White));
        assert!(board.square_is_attacked_by(str_to_index("h1").unwrap(), Color::White));
        board.white.pawns.set_bit(str_to_index("d1").unwrap()); // blocking pawn
        assert!(!board.square_is_attacked_by(str_to_index("h1").unwrap(), Color::White));
    }

    #[test]
    fn square_is_attacked_by_bishop() {
        let mut board = Board::cleared();
        board.white.bishops.set_bit(str_to_index("a1").unwrap());
        assert!(!board.square_is_attacked_by(str_to_index("b1").unwrap(), Color::White));
        assert!(!board.square_is_attacked_by(str_to_index("a2").unwrap(), Color::White));
        assert!(board.square_is_attacked_by(str_to_index("b2").unwrap(), Color::White));
    }

    #[test]
    fn square_is_attacked_by_queen() {
        let mut board = Board::cleared();
        board.black.queens.set_bit(str_to_index("a1").unwrap());
        assert!(board.square_is_attacked_by(str_to_index("b1").unwrap(), Color::Black));
        assert!(board.square_is_attacked_by(str_to_index("a2").unwrap(), Color::Black));
        assert!(board.square_is_attacked_by(str_to_index("b2").unwrap(), Color::Black));
        assert!(!board.square_is_attacked_by(!str_to_index("c2").unwrap(), Color::Black));
    }

    #[test]
    fn square_is_attacked_by_knight() {
        let mut board = Board::cleared();
        board.black.knights.set_bit(str_to_index("a1").unwrap());
        assert!(board.square_is_attacked_by(str_to_index("b3").unwrap(), Color::Black));
        assert!(board.square_is_attacked_by(str_to_index("c2").unwrap(), Color::Black));
        assert!(!board.square_is_attacked_by(str_to_index("c2").unwrap(), Color::White));
    }

    #[test]
    fn square_is_attacked_by_king() {
        let mut board = Board::cleared();
        board.black.king.set_bit(str_to_index("a1").unwrap());
        assert!(board.square_is_attacked_by(str_to_index("a2").unwrap(), Color::Black));
        assert!(board.square_is_attacked_by(str_to_index("b1").unwrap(), Color::Black));
        assert!(board.square_is_attacked_by(str_to_index("b2").unwrap(), Color::Black));
        assert!(!board.square_is_attacked_by(str_to_index("a3").unwrap(), Color::Black));
    }

    #[test]
    fn square_is_attacked_by_black_pawn() {
        let mut board = Board::cleared();
        board.black.pawns.set_bit(str_to_index("b7").unwrap());
        board.print();
        assert!(board.square_is_attacked_by(str_to_index("c6").unwrap(), Color::Black));
        assert!(board.square_is_attacked_by(str_to_index("a6").unwrap(), Color::Black));
        assert!(!board.square_is_attacked_by(str_to_index("b6").unwrap(), Color::Black));
    }

    #[test]
    fn square_is_attacked_by_white_pawn() {
        let mut board = Board::cleared();
        board.white.pawns.set_bit(str_to_index("a2").unwrap());
        board.print();
        assert!(board.square_is_attacked_by(str_to_index("b3").unwrap(), Color::White));
        assert!(!board.square_is_attacked_by(str_to_index("b1").unwrap(), Color::Black));
    }

    #[test]
    fn black_is_checked() {
        let mut board = Board::cleared();
        board.black.king.set_bit(str_to_index("a2").unwrap());
        assert!(!board.is_checked(Color::Black));
        board.white.rooks.set_bit(str_to_index("a1").unwrap());
        assert!(board.is_checked(Color::Black));
    }

    #[test]
    fn illegal_to_be_checked_after_move() {
        let mut board = Board::cleared();
        board.white.king.set_bit(str_to_index("a2").unwrap());
        board.black.rooks.set_bit(str_to_index("a1").unwrap());
        assert!(board.is_checked(Color::White));
        let fen = board.as_fen();
        board.print();
        assert!(!board.make_move("a2a3")); // illegal, still checked
        assert_eq!(fen, board.as_fen()); // unchanged
        assert_eq!(Color::White, board.turn); // unchanged
        assert!(board.make_move("a2b2")); // out of check
        assert_eq!(Color::Black, board.turn);
    }

    #[test]
    fn initial_moves() {
        let board = Board::initial_position();
        let num_opening_moves = board.white.count_moves(&board.black, Color::White);
        assert_eq!(20, num_opening_moves);
    }

    #[test]
    fn scholars_mate() {
        let board = Board::from_fen("r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR");
        let num_moves_in_mate = board.black.count_moves(&board.white, Color::Black);
        assert_eq!(0, num_moves_in_mate);
    }

    #[test]
    fn promotion_to_queen() {
        let mut board = Board::from_fen("K7/7P/8/8/8/8/8/k7 w -");
        assert!(board.make_move("h7h8q"));
        assert_eq!("K6Q/8/8/8/8/8/8/k7 b -", board.as_fen());
    }

    #[test]
    fn promotion_to_bishop() {
        let mut board = Board::from_fen("K7/7P/8/8/8/8/8/k7 w -");
        assert!(board.make_move("h7h8b"));
        assert_eq!("K6B/8/8/8/8/8/8/k7 b -", board.as_fen());
    }

    #[test]
    fn promotion_to_knight() {
        let mut board = Board::from_fen("K7/7P/8/8/8/8/8/k7 w -");
        assert!(board.make_move("h7h8n"));
        assert_eq!("K6N/8/8/8/8/8/8/k7 b -", board.as_fen());
    }

    #[test]
    fn promotion_to_rook() {
        let mut board = Board::from_fen("K7/7P/8/8/8/8/8/k7 w -");
        assert!(board.make_move("h7h8r"));
        assert_eq!("K6R/8/8/8/8/8/8/k7 b -", board.as_fen());
    }

    #[test]
    fn promotion_to_king_not_allowed() {
        let mut board = Board::from_fen("K7/7P/8/8/8/8/8/k7 w -");
        assert!(!board.make_move("h7h8k"));
        assert_eq!("K7/7P/8/8/8/8/8/k7 w -", board.as_fen());
    }

    #[test]
    fn missing_promotion() {
        let mut board = Board::from_fen("K7/7P/8/8/8/8/8/k7 w -");
        assert!(!board.make_move("h7h8"));
        assert_eq!("K7/7P/8/8/8/8/8/k7 w -", board.as_fen());
    }

    #[test]
    fn peft_test_position_1() {
        // https://gist.github.com/peterellisjones/8c46c28141c162d1d8a0f0badbc9cff9
        let board = Board::from_fen("2r5/3pk3/8/2P5/8/2K5/8/8");
        let move_count = board.white.count_moves(&board.black, Color::White);
        assert_eq!(9, move_count);
    }

    #[test]
    fn peft_test_position_2() {
        let board = Board::from_fen("r1bqkbnr/pppppppp/n7/8/8/P7/1PPPPPPP/RNBQKBNR");
        let move_count = board.white.count_moves(&board.black, Color::White);
        assert_eq!(19, move_count);
    }

    #[test]
    fn peft_test_position_3() {
        let board = Board::from_fen("rnb2k1r/pp1Pbppp/2p5/q7/2B5/8/PPPQNnPP/RNB1K2R");
        let move_count = board.white.count_moves(&board.black, Color::White);
        assert_eq!(38, move_count); // missing castling
    }

    #[test]
    fn max_moves_1() {
        // https://www.chessprogramming.org/Encoding_Moves
        let board = Board::from_fen("R6R/3Q4/1Q4Q1/4Q3/2Q4Q/Q4Q2/pp1Q4/kBNN1KB1");
        let move_count = board.white.count_moves(&board.black, Color::White);
        assert_eq!(218, move_count);
    }

    #[test]
    fn max_moves_2() {
        // https://www.chessprogramming.org/Encoding_Moves
        let board = Board::from_fen("3Q4/1Q4Q1/4Q3/2Q4R/Q4Q2/3Q4/1Q4Rp/1K1BBNNk");
        let move_count = board.white.count_moves(&board.black, Color::White);
        assert_eq!(218, move_count);
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
    use std::env;
    use std::io;
    use std::io::prelude::*;

    println!("Welcome to Rust Chess!");

    let mut board = if let Some(fen) = env::args().nth(1) {
        Board::from_fen(&fen)
    } else {
        Board::initial_position()
    };

    let print_state = |board: &Board| {
        println!("FEN: {}", board.as_fen());
        board.print();
        if board.is_checked(board.turn) {
            print!("Check! ");
        };
        println!("{:?}'s turn", board.turn);
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
