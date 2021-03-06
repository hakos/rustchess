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
pub enum Color {
    White,
    Black,
}

impl Color {
    pub fn other(self) -> Color {
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

const A1: u8 = 56;
const B1: u8 = 57;
const C1: u8 = 58;
const D1: u8 = 59;
const E1: u8 = 60;
const F1: u8 = 61;
const G1: u8 = 62;
const H1: u8 = 63;

const A8: u8 = 0;
const B8: u8 = 1;
const C8: u8 = 2;
const D8: u8 = 3;
const E8: u8 = 4;
const F8: u8 = 5;
const G8: u8 = 6;
const H8: u8 = 7;

const WHITE_KING_SIDE_CASTLING_SAFE_SQUARES: BitBoard = bitmask(E1) | bitmask(F1) | bitmask(G1);
const WHITE_KING_SIDE_CASTLING_EMPTY_SQUARES: BitBoard = bitmask(F1) | bitmask(G1);

const WHITE_QUEEN_SIDE_CASTLING_SAFE_SQUARES: BitBoard = bitmask(C1) | bitmask(D1) | bitmask(E1);
const WHITE_QUEEN_SIDE_CASTLING_EMPTY_SQUARES: BitBoard = bitmask(B1) | bitmask(C1) | bitmask(D1);

const BLACK_KING_SIDE_CASTLING_SAFE_SQUARES: BitBoard = bitmask(E8) | bitmask(F8) | bitmask(G8);
const BLACK_KING_SIDE_CASTLING_EMPTY_SQUARES: BitBoard = bitmask(F8) | bitmask(G8);

const BLACK_QUEEN_SIDE_CASTLING_SAFE_SQUARES: BitBoard = bitmask(C8) | bitmask(D8) | bitmask(E8);
const BLACK_QUEEN_SIDE_CASTLING_EMPTY_SQUARES: BitBoard = bitmask(B8) | bitmask(C8) | bitmask(D8);

fn is_white_king_side_castling(src: u8, dst: u8) -> bool {
    src == E1 && dst == G1
}
fn is_white_queen_side_castling(src: u8, dst: u8) -> bool {
    src == E1 && dst == C1
}
fn is_black_king_side_castling(src: u8, dst: u8) -> bool {
    src == E8 && dst == G8
}
fn is_black_queen_side_castling(src: u8, dst: u8) -> bool {
    src == E8 && dst == C8
}
fn is_queen_side_rook_square(index: u8, color: Color) -> bool {
    if color == Color::White {
        index == A1
    } else {
        index == A8
    }
}
fn is_king_side_rook_square(index: u8, color: Color) -> bool {
    if color == Color::White {
        index == H1
    } else {
        index == H8
    }
}
fn abs_diff(a: u8, b: u8) -> u8 {
    if a > b {
        a - b
    } else {
        b - a
    }
}
fn is_two_rank_move(src: u8, dst: u8) -> bool {
    abs_diff(src, dst) == 16
}
fn get_en_passant_square(src: u8, dst: u8) -> u8 {
    (src + dst) / 2
}
fn get_en_passant_capture_square(dst: u8, color: Color) -> u8 {
    if color == Color::White {
        dst + 8
    } else {
        dst - 8
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
            can_king_side_castle: false,
            can_queen_side_castle: false,
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
    }

    fn apply_move_impl(
        &mut self,
        enemies: &mut Pieces,
        src: u8,
        dst: u8,
        promotion: Option<Promotion>,
        en_passant_square: &mut Option<u8>,
        color: Color,
    ) -> bool {
        let mut next_en_passant_square = None;

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
                self.pawns.set_bit(dst);
            }

            self.pawns.clear_bit(src);
            if en_passant_square.is_some() && dst == en_passant_square.unwrap() {
                enemies.capture(get_en_passant_capture_square(dst, color));
            } else {
                enemies.capture(dst);
            }
            if is_two_rank_move(src, dst) {
                next_en_passant_square = Some(get_en_passant_square(src, dst));
            }
        } else if self.bishops.test_bit(src) {
            apply_move(&mut self.bishops, src, dst, enemies);
        } else if self.rooks.test_bit(src) {
            apply_move(&mut self.rooks, src, dst, enemies);
            if is_queen_side_rook_square(src, color) {
                self.can_queen_side_castle = false;
            } else if is_king_side_rook_square(src, color) {
                self.can_king_side_castle = false;
            }
        } else if self.knights.test_bit(src) {
            apply_move(&mut self.knights, src, dst, enemies);
        } else if self.queens.test_bit(src) {
            apply_move(&mut self.queens, src, dst, enemies);
        } else if self.king.test_bit(src) {
            apply_move(&mut self.king, src, dst, enemies);
            self.move_rook_if_castling(src, dst);
            self.can_king_side_castle = false;
            self.can_queen_side_castle = false;
        } else {
            panic!("Unknown piece at {}!", index_to_str(src));
        }

        // Capturing rook loses castling rights
        if is_queen_side_rook_square(dst, color.other()) {
            enemies.can_queen_side_castle = false;
        } else if is_king_side_rook_square(dst, color.other()) {
            enemies.can_king_side_castle = false;
        }

        *en_passant_square = next_en_passant_square;

        true
    }

    fn move_rook_if_castling(&mut self, src: u8, dst: u8) {
        if is_white_king_side_castling(src, dst) {
            self.rooks.clear_bit(H1);
            self.rooks.set_bit(F1);
        } else if is_white_queen_side_castling(src, dst) {
            self.rooks.clear_bit(A1);
            self.rooks.set_bit(D1);
        } else if is_black_king_side_castling(src, dst) {
            self.rooks.clear_bit(H8);
            self.rooks.set_bit(F8);
        } else if is_black_queen_side_castling(src, dst) {
            self.rooks.clear_bit(A8);
            self.rooks.set_bit(D8);
        }
    }

    fn verify_and_make_move(
        &mut self,
        enemies: &mut Pieces,
        m: Move,
        en_passant_square: &mut Option<u8>,
        color: Color,
    ) -> bool {
        let allowed_moves = self.get_moves(enemies, *en_passant_square, color)[m.src as usize];
        if allowed_moves.test_bit(m.dst) {
            self.apply_move_impl(enemies, m.src, m.dst, m.promotion, en_passant_square, color)
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
        for &direction in directions {
            let mut dst = src + direction;
            while dst.inside_board() && !friendly_pieces.test_bit(dst.to_index()) {
                let dst_index = dst.to_index();
                moves.set_bit(dst_index);
                let capture = enemy_pieces.test_bit(dst_index);
                if movement == Movement::Stepping || capture {
                    // Stop tracing this direction
                    break;
                }
                dst = dst + direction;
            }
        }
        moves
    }

    fn castling_squares_ok(
        &self,
        enemies: &Pieces,
        color: Color,
        required_empty: BitBoard,
        required_safe: BitBoard,
    ) -> bool {
        let empty = self.empty() & enemies.empty();
        let all_empty = required_empty & empty == required_empty;
        if !all_empty {
            return false;
        }

        let mut all_safe = true;
        required_safe.for_each_bit(|index| {
            if self.square_is_attacked_by(index, enemies, color.other()) {
                all_safe = false;
            }
        });
        all_safe
    }

    fn get_white_castling_moves(&self, enemies: &Pieces) -> BitBoard {
        let mut moves: BitBoard = 0;
        if self.can_king_side_castle
            && self.castling_squares_ok(
                enemies,
                Color::White,
                WHITE_KING_SIDE_CASTLING_EMPTY_SQUARES,
                WHITE_KING_SIDE_CASTLING_SAFE_SQUARES,
            )
        {
            moves.set_bit(G1);
        }
        if self.can_queen_side_castle
            && self.castling_squares_ok(
                enemies,
                Color::White,
                WHITE_QUEEN_SIDE_CASTLING_EMPTY_SQUARES,
                WHITE_QUEEN_SIDE_CASTLING_SAFE_SQUARES,
            )
        {
            moves.set_bit(C1);
        }
        moves
    }

    fn get_black_castling_moves(&self, enemies: &Pieces) -> BitBoard {
        let mut moves: BitBoard = 0;
        if self.can_king_side_castle
            && self.castling_squares_ok(
                enemies,
                Color::Black,
                BLACK_KING_SIDE_CASTLING_EMPTY_SQUARES,
                BLACK_KING_SIDE_CASTLING_SAFE_SQUARES,
            )
        {
            moves.set_bit(G8);
        }
        if self.can_queen_side_castle
            && self.castling_squares_ok(
                enemies,
                Color::Black,
                BLACK_QUEEN_SIDE_CASTLING_EMPTY_SQUARES,
                BLACK_QUEEN_SIDE_CASTLING_SAFE_SQUARES,
            )
        {
            moves.set_bit(C8);
        }
        moves
    }

    fn get_pseudo_legal_moves(
        &self,
        enemies: &Pieces,
        en_passant_square: Option<u8>,
        color: Color,
    ) -> [BitBoard; 64] {
        let mut moves: [BitBoard; 64] = [0; 64];

        self.pawns.for_each_bit(|src| {
            moves[src as usize] = if color == Color::White {
                self.get_white_pawn_moves(src, enemies, en_passant_square)
            } else {
                self.get_black_pawn_moves(src, enemies, en_passant_square)
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
        let king_moves =
            self.get_moves_from_square(king_index, &KING_QUEEN_MOVES, enemies, Movement::Stepping);
        let castling_moves = if color == Color::White {
            self.get_white_castling_moves(enemies)
        } else {
            self.get_black_castling_moves(enemies)
        };
        moves[king_index as usize] = king_moves | castling_moves;

        moves
    }

    fn get_moves(
        &self,
        enemies: &Pieces,
        en_passant_square: Option<u8>,
        color: Color,
    ) -> [BitBoard; 64] {
        let mut moves = self.get_pseudo_legal_moves(enemies, en_passant_square, color);
        self.remove_moves_that_leave_us_checked(&mut moves, enemies, en_passant_square, color);
        moves
    }

    fn count_moves(&self, moves: &[BitBoard; 64], color: Color) -> u32 {
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
        en_passant_square: Option<u8>,
        color: Color,
    ) {
        for (src, dsts) in moves.iter_mut().enumerate() {
            let dsts_copy = *dsts;
            dsts_copy.for_each_bit(|dst| {
                let mut enemies_copy = *enemies;
                let mut self_copy = *self;
                let mut en_passant_copy = en_passant_square;
                assert!(self_copy.apply_move_impl(
                    &mut enemies_copy,
                    src as u8,
                    dst,
                    // Arbitrary choice for check checking if this is a promotion move:
                    Some(Promotion::Queen),
                    &mut en_passant_copy,
                    color,
                ));
                if self_copy.is_checked_by(&enemies_copy, color.other()) {
                    dsts.clear_bit(dst);
                }
            });
        }
    }

    fn get_white_pawn_moves(
        &self,
        src: u8,
        enemies: &Pieces,
        en_passant_square: Option<u8>,
    ) -> BitBoard {
        const RANK_4: u64 = 0x00_00_00_ff_00_00_00_00;
        let empty = self.empty() & enemies.empty();
        let single_push = shift_north(bitmask(src)) & empty;
        let double_push = shift_north(single_push) & empty & RANK_4;
        let mut enemy_squares = enemies.occupancy();
        if let Some(en_passant) = en_passant_square {
            enemy_squares.set_bit(en_passant);
        }
        let captures =
            self.get_moves_from_square(src, &WHITE_PAWN_CAPTURES, enemies, Movement::Stepping)
                & enemy_squares;
        single_push | double_push | captures
    }

    fn get_black_pawn_moves(
        &self,
        src: u8,
        enemies: &Pieces,
        en_passant_square: Option<u8>,
    ) -> BitBoard {
        const RANK_5: u64 = 0x00_00_00_00_ff_00_00_00;
        let empty = self.empty() & enemies.empty();
        let single_push = shift_south(bitmask(src)) & empty;
        let double_push = shift_south(single_push) & empty & RANK_5;
        let mut enemy_squares = enemies.occupancy();
        if let Some(en_passant) = en_passant_square {
            enemy_squares.set_bit(en_passant);
        }
        let captures =
            self.get_moves_from_square(src, &BLACK_PAWN_CAPTURES, enemies, Movement::Stepping)
                & enemy_squares;
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

    fn count_doubled_pawns(&self) -> i32 {
        let mut count: i32 = 0;
        let mut remaining = self.pawns;
        while remaining != 0 {
            let index = remaining.first_bit();
            let file = Point::from_index(index).x as usize;
            let pawns_in_same_file = FILES[file] & remaining;
            count += pawns_in_same_file.count() as i32 - 1;
            remaining &= !pawns_in_same_file;
        }
        count
    }

    fn count_isolated_pawns(&self) -> i32 {
        let mut count: i32 = 0;
        self.pawns.for_each_bit(|index| {
            let file = Point::from_index(index).x as usize;
            let pawns_in_neighbor_files = NEIGHBOR_FILES[file] & self.pawns;
            if pawns_in_neighbor_files == 0 {
                count += 1
            }
        });
        count
    }

    fn square_scores(&self, color: Color, is_end_game: bool) -> i32 {
        pieces_square_score(self.rooks, &ROOK_SQUARE_SCORES, color)
            + pieces_square_score(self.bishops, &BISHOP_SQUARE_SCORES, color)
            + pieces_square_score(self.knights, &KNIGHT_SQUARE_SCORES, color)
            + pieces_square_score(self.pawns, &PAWN_SQUARE_SCORES, color)
            + pieces_square_score(
                self.king,
                if is_end_game {
                    &KING_END_GAME_SQUARE_SCORES
                } else {
                    &KING_MIDDLE_GAME_SQUARE_SCORES
                },
                color,
            )
    }
}

#[derive(Copy, Clone)]
pub struct Board {
    white: Pieces,
    black: Pieces,
    turn: Color,
    en_passant_square: Option<u8>,
}

#[derive(Debug, PartialEq, Copy, Clone)]
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

    fn to_index(self) -> u8 {
        assert!(self.inside_board());
        self.y as u8 * 8 + self.x as u8
    }

    fn inside_board(self) -> bool {
        self.x >= 0 && self.x < 8 && self.y >= 0 && self.y < 8
    }
}

use std::ops::Add;

impl Add for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
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

#[cfg_attr(rustfmt, rustfmt_skip)]
const PAWN_SQUARE_SCORES: [i32; 64] = [
     0,  0,  0,  0,  0,  0,  0,  0,
    50, 50, 50, 50, 50, 50, 50, 50,
    10, 10, 20, 30, 30, 20, 10, 10,
     5,  5, 10, 25, 25, 10,  5,  5,
     0,  0,  0, 20, 20,  0,  0,  0,
     5, -5,-10,  0,  0,-10, -5,  5,
     5, 10, 10,-20,-20, 10, 10,  5,
     0,  0,  0,  0,  0,  0,  0,  0,
];

#[cfg_attr(rustfmt, rustfmt_skip)]
const KNIGHT_SQUARE_SCORES: [i32; 64] = [
    -50,-40,-30,-30,-30,-30,-40,-50,
    -40,-20,  0,  0,  0,  0,-20,-40,
    -30,  0, 10, 15, 15, 10,  0,-30,
    -30,  5, 15, 20, 20, 15,  5,-30,
    -30,  0, 15, 20, 20, 15,  0,-30,
    -30,  5, 10, 15, 15, 10,  5,-30,
    -40,-20,  0,  5,  5,  0,-20,-40,
    -50,-40,-30,-30,-30,-30,-40,-50,
];

#[cfg_attr(rustfmt, rustfmt_skip)]
const BISHOP_SQUARE_SCORES: [i32; 64] = [
    -20,-10,-10,-10,-10,-10,-10,-20,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5, 10, 10,  5,  0,-10,
    -10,  5,  5, 10, 10,  5,  5,-10,
    -10,  0, 10, 10, 10, 10,  0,-10,
    -10, 10, 10, 10, 10, 10, 10,-10,
    -10,  5,  0,  0,  0,  0,  5,-10,
    -20,-10,-10,-10,-10,-10,-10,-20,
];

#[cfg_attr(rustfmt, rustfmt_skip)]
const ROOK_SQUARE_SCORES: [i32; 64] = [
     0,  0,  0,  0,  0,  0,  0,  0,
     5, 10, 10, 10, 10, 10, 10,  5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
     0,  0,  0,  5,  5,  0,  0,  0,
];

#[cfg_attr(rustfmt, rustfmt_skip)]
const KING_MIDDLE_GAME_SQUARE_SCORES: [i32; 64] = [
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -20,-30,-30,-40,-40,-30,-30,-20,
    -10,-20,-20,-20,-20,-20,-20,-10,
     20, 20,  0,  0,  0,  0, 20, 20,
     20, 50, 20,  0,  0, 20, 50, 20,
];

#[cfg_attr(rustfmt, rustfmt_skip)]
const KING_END_GAME_SQUARE_SCORES: [i32; 64] = [
    -50,-40,-30,-20,-20,-30,-40,-50,
    -30,-20,-10,  0,  0,-10,-20,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-30,  0,  0,  0,  0,-30,-30,
    -50,-30,-30,-30,-30,-30,-30,-50,
];

fn pieces_square_score(pieces: BitBoard, scores: &[i32; 64], color: Color) -> i32 {
    let mut score: i32 = 0;
    pieces.for_each_bit(|square| {
        let index = if color == Color::White {
            square
        } else {
            63 - square
        };
        score += scores[index as usize];
    });
    score
}

#[cfg_attr(feature = "cargo-clippy", allow(clippy::large_digit_groups))]
const FILES: [BitBoard; 8] = [
    0b00000001_00000001_00000001_00000001_00000001_00000001_00000001_00000001,
    0b00000010_00000010_00000010_00000010_00000010_00000010_00000010_00000010,
    0b00000100_00000100_00000100_00000100_00000100_00000100_00000100_00000100,
    0b00001000_00001000_00001000_00001000_00001000_00001000_00001000_00001000,
    0b00010000_00010000_00010000_00010000_00010000_00010000_00010000_00010000,
    0b00100000_00100000_00100000_00100000_00100000_00100000_00100000_00100000,
    0b01000000_01000000_01000000_01000000_01000000_01000000_01000000_01000000,
    0b10000000_10000000_10000000_10000000_10000000_10000000_10000000_10000000,
];

#[cfg_attr(feature = "cargo-clippy", allow(clippy::large_digit_groups))]
const NEIGHBOR_FILES: [BitBoard; 8] = [
    0b00000010_00000010_00000010_00000010_00000010_00000010_00000010_00000010,
    0b00000101_00000101_00000101_00000101_00000101_00000101_00000101_00000101,
    0b00001010_00001010_00001010_00001010_00001010_00001010_00001010_00001010,
    0b00010100_00010100_00010100_00010100_00010100_00010100_00010100_00010100,
    0b00101000_00101000_00101000_00101000_00101000_00101000_00101000_00101000,
    0b01010000_01010000_01010000_01010000_01010000_01010000_01010000_01010000,
    0b10100000_10100000_10100000_10100000_10100000_10100000_10100000_10100000,
    0b01000000_01000000_01000000_01000000_01000000_01000000_01000000_01000000,
];

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

#[derive(PartialEq, Copy, Clone)]
pub struct Move {
    pub src: u8,
    pub dst: u8,
    promotion: Option<Promotion>,
}

use std::fmt;

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}{}",
            index_to_str(self.src),
            index_to_str(self.dst),
            match self.promotion {
                Some(Promotion::Queen) => "q",
                Some(Promotion::Bishop) => "b",
                Some(Promotion::Rook) => "r",
                Some(Promotion::Knight) => "k",
                None => "",
            }
        )
    }
}

impl fmt::Debug for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub struct MoveIterator {
    moves: [BitBoard; 64],
    pawns: BitBoard,
    src: u8,
    dst: u8,
    color: Color,
    promotion_iter: std::slice::Iter<'static, Promotion>,
}

const ALL_PROMOTIONS: [Promotion; 4] = [
    Promotion::Queen,
    Promotion::Rook,
    Promotion::Knight,
    Promotion::Bishop,
];

const NO_PROMOTIONS: [Promotion; 0] = [];

impl Iterator for MoveIterator {
    type Item = Move;

    fn next(&mut self) -> Option<Move> {
        if let Some(&promotion) = self.promotion_iter.next() {
            return Some(Move {
                src: self.src,
                dst: self.dst,
                promotion: Some(promotion),
            });
        }

        // Scan to next source square with moves
        while self.src < self.moves.len() as u8 && self.moves[self.src as usize] == 0 {
            self.src += 1
        }

        if self.src == self.moves.len() as u8 {
            // No more moves
            return None;
        }

        // Consume next destination from this source
        let moves = &mut self.moves[self.src as usize];
        self.dst = moves.first_bit();
        moves.clear_bit(self.dst);

        if self.pawns.test_bit(self.src as u8) && is_promotion_rank(self.dst, self.color) {
            // Start iterating over all promotions
            self.promotion_iter = ALL_PROMOTIONS.iter();
            let &promotion = self.promotion_iter.next().unwrap();
            return Some(Move {
                src: self.src,
                dst: self.dst,
                promotion: Some(promotion),
            });
        }

        Some(Move {
            src: self.src,
            dst: self.dst,
            promotion: None,
        })
    }
}

impl Board {
    pub fn initial_position() -> Board {
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
            en_passant_square: None,
        }
    }

    fn cleared() -> Board {
        Board {
            white: Pieces::cleared(),
            black: Pieces::cleared(),
            turn: Color::White,
            en_passant_square: None,
        }
    }

    pub fn count_pieces(&self) -> u32 {
        (self.white.occupancy() | self.black.occupancy()).count()
    }


    pub fn from_fen(fen: &str) -> Board {
        let mut board = Board::cleared();
        let mut fen_parts = fen.split(' ');

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

        match fen_parts.next() {
            Some("-") => (),
            Some(en_passant) => {
                board.en_passant_square = str_to_index(en_passant);
                if board.en_passant_square.is_none() {
                    panic!(
                        "Unknown en passant target square '{}' in FEN string",
                        en_passant
                    );
                }
            }
            None => (),
        }

        board
    }

    pub fn as_fen(&self) -> String {
        let position = self
            .as_unicode()
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

        let en_passant = match self.en_passant_square {
            Some(square) => index_to_str(square),
            None => String::from("-"),
        };

        format!("{} {} {} {}", position, turn, castling, en_passant)
    }

    fn as_unicode(&self) -> [char; 64] {
        let mut chars = ['.'; 64];

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

    pub fn print(&self) {
        print_unicode_board(&self.as_unicode());
    }

    pub fn make_move(&mut self, m: &str) -> bool {
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

        let m = Move {
            src,
            dst,
            promotion,
        };

        // Copy to be able to restore if move ends up in check
        let self_before_move = *self;

        if self.turn == Color::White {
            if !self.white.verify_and_make_move(
                &mut self.black,
                m,
                &mut self.en_passant_square,
                self.turn,
            ) {
                return false;
            }
        } else if !self.black.verify_and_make_move(
            &mut self.white,
            m,
            &mut self.en_passant_square,
            self.turn,
        ) {
            return false;
        }

        if self.is_checked() {
            // Restore board
            *self = self_before_move;
            return false;
        }

        self.change_turn();

        true
    }

    pub fn change_turn(&mut self) {
        self.turn = self.turn.other();
    }

    pub fn make_move_unverified(&mut self, m: Move) {
        if self.turn == Color::White {
            assert!(self.white.apply_move_impl(
                &mut self.black,
                m.src,
                m.dst,
                m.promotion,
                &mut self.en_passant_square,
                self.turn
            ));
        } else {
            assert!(self.black.apply_move_impl(
                &mut self.white,
                m.src,
                m.dst,
                m.promotion,
                &mut self.en_passant_square,
                self.turn
            ));
        }
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

    fn get_moves(&self) -> [BitBoard; 64] {
        if self.turn == Color::White {
            self.white
                .get_moves(&self.black, self.en_passant_square, self.turn)
        } else {
            self.black
                .get_moves(&self.white, self.en_passant_square, self.turn)
        }
    }

    pub fn count_moves(&self) -> u32 {
        if self.turn == Color::White {
            self.white.count_moves(&self.get_moves(), self.turn)
        } else {
            self.black.count_moves(&self.get_moves(), self.turn)
        }
    }

    pub fn is_checked(&self) -> bool {
        if self.turn == Color::White {
            self.white.is_checked_by(&self.black, Color::Black)
        } else {
            self.black.is_checked_by(&self.white, Color::White)
        }
    }

    pub fn is_check_mated(&self) -> bool {
        self.is_checked() && self.count_moves() == 0
    }

    pub fn is_stale_mate(&self) -> bool {
        !self.is_checked() && self.count_moves() == 0
    }

    pub fn turn(&self) -> Color {
        self.turn
    }

    pub fn pseudo_legal_move_iter(&self) -> MoveIterator {
        let (myself, opponent) = self.myself_opponent();
        MoveIterator {
            moves: myself.get_pseudo_legal_moves(opponent, self.en_passant_square, self.turn),
            pawns: myself.pawns,
            src: 0,
            dst: 0,
            promotion_iter: NO_PROMOTIONS.iter(),
            color: self.turn,
        }
    }

    fn myself_opponent(&self) -> (&Pieces, &Pieces) {
        if self.turn == Color::White {
            (&self.white, &self.black)
        } else {
            (&self.black, &self.white)
        }
    }

    pub fn evaluate(&self) -> i32 {
        let (myself, opponent) = self.myself_opponent();

        const QUEEN_VALUE: i32 = 900;
        const ROOK_VALUE: i32 = 500;
        const BISHOP_VALUE: i32 = 330;
        const KNIGHT_VALUE: i32 = 320;
        const PAWN_VALUE: i32 = 100;

        let material = QUEEN_VALUE
            * (myself.queens.count() as i32 - opponent.queens.count() as i32)
            + ROOK_VALUE * (myself.rooks.count() as i32 - opponent.rooks.count() as i32)
            + BISHOP_VALUE * (myself.bishops.count() as i32 - opponent.bishops.count() as i32)
            + KNIGHT_VALUE * (myself.knights.count() as i32 - opponent.knights.count() as i32)
            + PAWN_VALUE * (myself.pawns.count() as i32 - opponent.pawns.count() as i32);

        let total_material = QUEEN_VALUE * (myself.queens | opponent.queens).count() as i32
            + ROOK_VALUE * (myself.rooks | opponent.rooks).count() as i32
            + BISHOP_VALUE * (myself.bishops | opponent.bishops).count() as i32
            + KNIGHT_VALUE * (myself.knights | opponent.knights).count() as i32;

        let is_end_game = total_material < 3000;

        let position = myself.square_scores(self.turn, is_end_game)
            - opponent.square_scores(self.turn.other(), is_end_game);

        let pawn_structure = -30
            * (myself.count_doubled_pawns() as i32 - opponent.count_doubled_pawns())
            - 30 * (myself.count_isolated_pawns() as i32 - opponent.count_isolated_pawns());

        material + position + pawn_structure
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

fn index_to_str(index: u8) -> String {
    let point = Point::from_index(index);
    assert!(point.inside_board());
    format!("{}{}", char::from(b'a' + point.x as u8), 8 - point.y)
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
        '.' => ' ',
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
            "♜♞♝♛♚♝♞♜♟♟♟♟♟♟♟♟................................♙♙♙♙♙♙♙♙♖♘♗♕♔♗♘♖",
            Board::initial_position().as_unicode().iter().collect::<String>()
        );
    }

    #[test]
    fn initial_board_fen() {
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -",
            Board::initial_position().as_fen()
        );
    }

    #[test]
    fn parse_fen() {
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3",
            Board::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3").as_fen(),
        );
    }

    #[test]
    fn count_doubled_pawns() {
        assert_eq!(0, Board::initial_position().white.count_doubled_pawns());
        assert_eq!(
            1,
            Board::from_fen("rnbqkbnr/ppp1pppp/8/8/3Pp3/8/PPP2PPP/RNBQKBNR w KQkq - 0 3")
                .black
                .count_doubled_pawns()
        );
        assert_eq!(
            2,
            Board::from_fen("rnbqkbnr/ppp1p1pp/8/4p3/3Pp3/8/PPP2PPP/RN1QKBNR w KQkq - 0 5")
                .black
                .count_doubled_pawns()
        );
    }

    #[test]
    fn count_isolated_pawns() {
        assert_eq!(0, Board::initial_position().white.count_isolated_pawns());
        let test = Board::from_fen("rnbqkbnr/ppp4p/8/4p1P1/3pP3/8/PPP3P1/RN1QKBNR b KQkq - 0 8");
        assert_eq!(1, test.black.count_isolated_pawns());
        assert_eq!(3, test.white.count_isolated_pawns());
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
        assert!(!board.make_move("a2b3")); // no capture
        assert!(!board.make_move("e2e5")); // too far
        assert!(board.make_move("e2e4"));
        assert!(board.make_move("e7e5"));
        assert!(!board.make_move("e4e5")); // occupied
        assert_eq!(
            "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6",
            board.as_fen()
        );
    }

    #[test]
    fn opening_moves_fen() {
        let mut board = Board::initial_position();
        assert!(board.make_move("e2e4"));
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3",
            board.as_fen()
        );
        assert!(board.make_move("c7c5"));
        assert_eq!(
            "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6",
            board.as_fen()
        );
        assert!(board.make_move("g1f3"));
        assert_eq!(
            "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq -",
            board.as_fen()
        );
    }

    #[test]
    fn move_bishop() {
        let mut board = Board::initial_position();
        assert!(board.make_move("d2d4"));
        assert!(!board.make_move("c1c2"));
        board.turn = Color::White;
        assert!(board.make_move("c1d2"));
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/3P4/8/PPPBPPPP/RN1QKBNR b KQkq -",
            board.as_fen()
        );
    }

    #[test]
    fn move_rook_and_lose_castling_right() {
        let mut board = Board::initial_position();
        assert!(!board.make_move("a1a2"));
        assert!(!board.make_move("a1b1"));
        assert!(board.make_move("a2a4"));
        board.turn = Color::White;
        assert!(board.make_move("a1a2"));
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/P7/8/RPPPPPPP/1NBQKBNR b Kkq -",
            board.as_fen()
        );
    }

    #[test]
    fn move_knight() {
        let mut board = Board::initial_position();
        assert!(board.make_move("b1c3"));
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/R1BQKBNR b KQkq -",
            board.as_fen()
        );
        assert_eq!(Color::Black, board.turn);
        assert!(board.make_move("b8c6"));
    }

    #[test]
    fn move_king_and_lose_castling_rights() {
        let mut board = Board::initial_position();
        assert!(board.make_move("e2e4"));
        board.turn = Color::White;
        assert!(!board.make_move("e1e3")); // too far
        assert!(board.make_move("e1e2"));
        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPPKPPP/RNBQ1BNR b kq -",
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
            "rnbqkbnr/pppppppp/8/8/3P4/3Q4/PPP1PPPP/RNB1KBNR b KQkq -",
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
        board.turn = Color::Black;
        assert!(!board.is_checked());
        board.white.rooks.set_bit(str_to_index("a1").unwrap());
        assert!(board.is_checked());
    }

    #[test]
    fn illegal_to_be_checked_after_move() {
        let mut board = Board::cleared();
        board.white.king.set_bit(str_to_index("a2").unwrap());
        board.black.rooks.set_bit(str_to_index("a1").unwrap());
        assert!(board.is_checked());
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
        let num_opening_moves = board.count_moves();
        assert_eq!(20, num_opening_moves);
    }

    #[test]
    fn scholars_mate() {
        let board = Board::from_fen("r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b");
        let num_moves_in_mate = board.count_moves();
        assert_eq!(0, num_moves_in_mate);
        assert!(board.is_check_mated());
    }

    #[test]
    fn promotion_to_queen() {
        let mut board = Board::from_fen("K7/7P/8/8/8/8/8/k7 w -");
        assert!(board.make_move("h7h8q"));
        assert_eq!("K6Q/8/8/8/8/8/8/k7 b - -", board.as_fen());
    }

    #[test]
    fn promotion_to_bishop() {
        let mut board = Board::from_fen("K7/7P/8/8/8/8/8/k7 w -");
        assert!(board.make_move("h7h8b"));
        assert_eq!("K6B/8/8/8/8/8/8/k7 b - -", board.as_fen());
    }

    #[test]
    fn promotion_to_knight() {
        let mut board = Board::from_fen("K7/7P/8/8/8/8/8/k7 w -");
        assert!(board.make_move("h7h8n"));
        assert_eq!("K6N/8/8/8/8/8/8/k7 b - -", board.as_fen());
    }

    #[test]
    fn promotion_to_rook() {
        let mut board = Board::from_fen("K7/7P/8/8/8/8/8/k7 w -");
        assert!(board.make_move("h7h8r"));
        assert_eq!("K6R/8/8/8/8/8/8/k7 b - -", board.as_fen());
    }

    #[test]
    fn promotion_to_king_not_allowed() {
        let mut board = Board::from_fen("K7/7P/8/8/8/8/8/k7 w -");
        assert!(!board.make_move("h7h8k"));
        assert_eq!("K7/7P/8/8/8/8/8/k7 w - -", board.as_fen());
    }

    #[test]
    fn missing_promotion() {
        let mut board = Board::from_fen("K7/7P/8/8/8/8/8/k7 w -");
        assert!(!board.make_move("h7h8"));
        assert_eq!("K7/7P/8/8/8/8/8/k7 w - -", board.as_fen());
    }

    #[test]
    fn capture_rook_loses_castling_rights() {
        let mut board = Board::from_fen("r3k2r/Rb4bq/8/8/8/8/7B/4K2R b Kkq - 1 1");
        board.make_move("b7h1");
        assert!(!board.white.can_king_side_castle);
    }

    #[test]
    fn reavealing_own_king_in_check_by_promoting_pawn_not_allowed() {
        let board = Board::from_fen("r1bq1k1r/pp1Pbppp/n1p5/8/2B5/8/PPPKNnPP/RNBQ3R w - -");
        assert_eq!(32, board.count_moves());
    }

    #[test]
    fn white_king_side_castling() {
        let mut board = Board::from_fen("rnb2k1r/pp1Pbppp/2p5/q7/2B5/8/PPPQNnPP/RNB1K2R w QK");
        assert!(board.make_move("e1g1"));
        assert_eq!(
            "rnb2k1r/pp1Pbppp/2p5/q7/2B5/8/PPPQNnPP/RNB2RK1 b - -",
            board.as_fen()
        );
    }

    #[test]
    fn white_queen_side_castling() {
        let mut board =
            Board::from_fen("r3k2r/pppqbppp/2n1pn2/3p1b2/3P1B2/2N1PN2/PPPQBPPP/R3K2R w KQkq");
        assert!(board.make_move("e1c1"));
        assert_eq!(
            "r3k2r/pppqbppp/2n1pn2/3p1b2/3P1B2/2N1PN2/PPPQBPPP/2KR3R b kq -",
            board.as_fen()
        );
    }

    #[test]
    fn black_king_side_castling() {
        let mut board =
            Board::from_fen("r3k2r/pppqbppp/2n1pn2/3p1b2/3P1B2/2N1PN2/PPPQBPPP/2KR3R b kq");
        assert!(board.make_move("e8g8"));
        assert_eq!(
            "r4rk1/pppqbppp/2n1pn2/3p1b2/3P1B2/2N1PN2/PPPQBPPP/2KR3R w - -",
            board.as_fen()
        );
    }

    #[test]
    fn black_queen_side_castling() {
        let mut board =
            Board::from_fen("r3k2r/pppqbppp/2n1pn2/3p1b2/3P1B2/2N1PN2/PPPQBPPP/2KR3R b kq");
        assert!(board.make_move("e8c8"));
        assert_eq!(
            "2kr3r/pppqbppp/2n1pn2/3p1b2/3P1B2/2N1PN2/PPPQBPPP/2KR3R w - -",
            board.as_fen()
        );
    }

    #[test]
    fn moving_rook_loses_queen_side_castling_rights() {
        let mut board = Board::from_fen("r1bqkbnr/ppppppp1/2n5/7p/P7/5N2/1PPPPPPP/RNBQKB1R w KQkq");

        assert!(board.white.can_queen_side_castle);
        assert!(board.make_move("a1a2"));
        assert!(!board.white.can_queen_side_castle);

        assert!(board.black.can_queen_side_castle);
        assert!(board.make_move("a8b8"));
        assert!(!board.black.can_queen_side_castle);
    }

    #[test]
    fn moving_rook_loses_king_side_castling_rights() {
        let mut board = Board::from_fen("r1bqkbnr/ppppppp1/2n5/7p/P7/5N2/1PPPPPPP/RNBQKB1R w KQkq");

        assert!(board.white.can_king_side_castle);
        assert!(board.make_move("h1g1"));
        assert!(!board.white.can_king_side_castle);

        assert!(board.black.can_king_side_castle);
        assert!(board.make_move("h8h6"));
        assert!(!board.black.can_king_side_castle);
    }

    #[test]
    fn max_moves_1() {
        // https://www.chessprogramming.org/Encoding_Moves
        let board = Board::from_fen("R6R/3Q4/1Q4Q1/4Q3/2Q4Q/Q4Q2/pp1Q4/kBNN1KB1");
        assert_eq!(218, board.count_moves());
    }

    #[test]
    fn max_moves_2() {
        // https://www.chessprogramming.org/Encoding_Moves
        let board = Board::from_fen("3Q4/1Q4Q1/4Q3/2Q4R/Q4Q2/3Q4/1Q4Rp/1K1BBNNk");
        assert_eq!(218, board.count_moves());
    }

    #[test]
    fn black_en_passant() {
        let mut board = Board::from_fen("8/8/8/2k5/2pP4/8/B7/4K3 b - d3");
        assert!(board.make_move("c4d3"));
        assert_eq!("8/8/8/2k5/8/3p4/B7/4K3 w - -", board.as_fen());
    }

    #[test]
    fn white_en_passant() {
        let mut board =
            Board::from_fen("rnbqkb1r/pp1ppppp/5n2/2pP4/8/8/PPP1PPPP/RNBQKBNR w KQkq c6");
        assert!(board.make_move("d5c6"));
        assert_eq!(
            "rnbqkb1r/pp1ppppp/2P2n2/8/8/8/PPP1PPPP/RNBQKBNR b KQkq -",
            board.as_fen()
        )
    }

    #[test]
    fn test_str_to_index() {
        assert!(str_to_index("a0").is_none());
        assert!(str_to_index("a9").is_none());
        assert!(str_to_index("i1").is_none());
        assert!(str_to_index("91").is_none());

        assert_eq!(A8, str_to_index("a8").unwrap());
        assert_eq!(H8, str_to_index("h8").unwrap());
        assert_eq!(A1, str_to_index("a1").unwrap());
        assert_eq!(H1, str_to_index("h1").unwrap());
    }

    #[test]
    fn test_str_to_index_to_str() {
        assert_eq!("a1", index_to_str(str_to_index("a1").unwrap()));
        assert_eq!("h8", index_to_str(str_to_index("h8").unwrap()));
    }

    #[test]
    fn stale_mate() {
        let board = Board::from_fen("5bnr/4p1pq/4Qpkr/7p/2P4P/8/PP1PPPP1/RNB1KBNR b KQ - 0 10");
        assert!(board.is_stale_mate());
    }
}
