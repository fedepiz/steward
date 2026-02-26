#[derive(Clone, Copy, PartialEq, PartialOrd, Default, Debug)]
pub struct V2 {
    pub x: f32,
    pub y: f32,
}

impl V2 {
    #[inline]
    pub const fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }

    #[inline]
    pub const fn splat(v: f32) -> Self {
        Self::new(v, v)
    }

    pub fn magnitude(self) -> f32 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }

    pub fn min(self, other: V2) -> V2 {
        V2::new(self.x.min(other.x), self.y.min(other.y))
    }
}

impl From<(f32, f32)> for V2 {
    fn from((x, y): (f32, f32)) -> Self {
        Self::new(x, y)
    }
}

impl std::ops::Index<usize> for V2 {
    type Output = f32;

    fn index(&self, index: usize) -> &Self::Output {
        match index {
            0 => &self.x,
            1 => &self.y,
            _ => {
                debug_assert!(false);
                &0.
            }
        }
    }
}

impl std::ops::IndexMut<usize> for V2 {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        match index {
            0 => &mut self.x,
            1 => &mut self.y,
            _ => {
                debug_assert!(false);
                &mut self.x
            }
        }
    }
}

impl std::ops::Add for V2 {
    type Output = V2;
    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.x + rhs.x, self.y + rhs.y)
    }
}

impl std::ops::AddAssign for V2 {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl std::ops::Sub for V2 {
    type Output = V2;
    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.x - rhs.x, self.y - rhs.y)
    }
}

impl std::ops::SubAssign for V2 {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl std::ops::Mul for V2 {
    type Output = V2;
    fn mul(self, rhs: Self) -> Self::Output {
        Self::new(self.x * rhs.x, self.y * rhs.y)
    }
}

impl std::ops::Mul<f32> for V2 {
    type Output = V2;

    fn mul(self, rhs: f32) -> Self::Output {
        V2::new(self.x * rhs, self.y * rhs)
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Default)]
pub struct Rect {
    pub x: f32,
    pub y: f32,
    pub w: f32,
    pub h: f32,
}

impl Rect {
    pub fn with_position(self, pos: V2) -> Self {
        Self {
            x: pos.x,
            y: pos.y,
            ..self
        }
    }
    pub fn contains(&self, pos: V2) -> bool {
        pos.x >= self.x && pos.x <= self.x + self.w && pos.y >= self.y && pos.y <= self.y + self.h
    }
    pub fn size(&self) -> V2 {
        V2::new(self.w, self.h)
    }
}
