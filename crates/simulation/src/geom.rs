#[derive(Default, Clone, Copy, PartialEq, PartialOrd, Debug)]
pub(crate) struct V2 {
    pub x: f32,
    pub y: f32,
}

impl V2 {
    pub(crate) fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }

    pub(crate) fn splat(v: f32) -> Self {
        Self { x: v, y: v }
    }

    pub(crate) fn normalize(self) -> V2 {
        let n = self.magnitude();
        if n == 0. {
            return V2::default();
        }
        V2::new(self.x / n, self.y / n)
    }

    pub(crate) fn magnitude(self) -> f32 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }

    pub fn distance(p1: V2, p2: V2) -> f32 {
        Self::distance_squared(p1, p2).sqrt()
    }

    pub fn distance_squared(p1: V2, p2: V2) -> f32 {
        (p1.x - p2.x).powi(2) + (p1.y - p2.y).powi(2)
    }

    pub fn lerp(p1: V2, p2: V2, t: f32) -> V2 {
        let t = t.clamp(0., 1.);
        V2::new(lerp(p1.x, p2.x, t), lerp(p1.y, p2.y, t))
    }

    pub fn round(self) -> V2 {
        V2::new(self.x.round(), self.y.round())
    }

    pub fn as_pair(self) -> (f32, f32) {
        (self.x, self.y)
    }
}

fn lerp(a: f32, b: f32, t: f32) -> f32 {
    a + (b - a) * t
}

impl std::ops::Add<V2> for V2 {
    type Output = V2;
    fn add(self, rhs: V2) -> Self::Output {
        V2::new(self.x + rhs.x, self.y + rhs.y)
    }
}

impl std::ops::Sub<V2> for V2 {
    type Output = V2;
    fn sub(self, rhs: V2) -> Self::Output {
        V2::new(self.x - rhs.x, self.y - rhs.y)
    }
}

impl std::ops::Mul<f32> for V2 {
    type Output = V2;

    fn mul(self, rhs: f32) -> Self::Output {
        V2::new(self.x * rhs, self.y * rhs)
    }
}
