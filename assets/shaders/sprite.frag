#version 330
precision lowp float;

in vec2 uv;
out vec4 frag_color;

uniform sampler2D atlas_texture;
uniform vec4 border_highlight;
uniform float time;
uniform float pulse_intensity;
uniform float transparency_intensity;

// Border thickness in source texture pixels.
// This is a shader-side constant on purpose: it keeps the effect stable and
// easy to tweak without adding more CPU-side parameters yet.
const int BORDER_THICKNESS_PX = 3;
// Pulsation speed in radians/second for the sine wave.
const float PULSE_SPEED = 4.5;
// Maximum fraction of white mixed into the sprite color at full pulse.
const float MAX_PULSE_BRIGHTEN = 0.1;

// Pixels with alpha above this are treated as part of the sprite silhouette.
const float SOLID_ALPHA_THRESHOLD = 0.01;


// Helper: detect whether a UV point is within BORDER_THICKNESS_PX of any solid
// sprite texel. We search neighbors in a circular radius for smoother corners.
bool is_near_solid_edge(vec2 at_uv) {
    vec2 texel = 1.0 / vec2(textureSize(atlas_texture, 0));

    for (int y = -BORDER_THICKNESS_PX; y <= BORDER_THICKNESS_PX; y++) {
        for (int x = -BORDER_THICKNESS_PX; x <= BORDER_THICKNESS_PX; x++) {
            if (x == 0 && y == 0) {
                continue;
            }

            float distance_px = length(vec2(float(x), float(y)));
            if (distance_px > float(BORDER_THICKNESS_PX)) {
                continue;
            }

            vec2 sample_uv = at_uv + vec2(float(x), float(y)) * texel;
            float neighbor_alpha = texture(atlas_texture, sample_uv).a;
            if (neighbor_alpha > SOLID_ALPHA_THRESHOLD) {
                return true;
            }
        }
    }

    return false;
}

// Pass: apply time-based pulsation and transparency to sprite interior.
// `pulse_intensity` and `transparency_intensity` are expected in [0, 1].
// - 0 disables the corresponding effect
// - 1 uses full configured amplitude
vec4 sample_pulsed_sprite_color(vec4 base_color) {
    bool base_is_solid = base_color.a > SOLID_ALPHA_THRESHOLD;

    // Sine wave remapped from [-1, 1] to [0, 1] so intensity never goes negative.
    float pulse_wave = 0.5 + 0.5 * sin(time * PULSE_SPEED);

    // Final brighten amount, clamped for safety to avoid overshooting.
    float pulse_amount = clamp(pulse_intensity, 0.0, 1.0) * pulse_wave * MAX_PULSE_BRIGHTEN;
    // Fade amount in [0, 1]. At full intensity, the sprite alpha oscillates from
    // fully opaque to fully transparent and back.
    float fade_amount = clamp(transparency_intensity, 0.0, 1.0) * pulse_wave;

    // Pulsation affects only visible sprite pixels, not transparent background.
    vec3 pulsed_rgb = mix(base_color.rgb, vec3(1.0), pulse_amount);
    float pulsed_alpha = base_color.a * (1.0 - fade_amount);
    return base_is_solid ? vec4(pulsed_rgb, pulsed_alpha) : base_color;
}

// Pass: compute border highlight color for this fragment.
// The border is only emitted on transparent pixels near the silhouette.
vec4 sample_border_highlight(vec2 at_uv, vec4 base_color) {
    bool is_transparent = base_color.a <= SOLID_ALPHA_THRESHOLD;
    bool near_edge = is_near_solid_edge(at_uv);
    bool draw_border = is_transparent && near_edge;

    return draw_border ? border_highlight : vec4(0.0, 0.0, 0.0, 0.0);
}

// Pass: compose the final color.
// Current policy:
// - Keep pulsed sprite interior for solid texels.
// - Replace transparent edge pixels with border highlight.
vec4 combine_passes(vec4 base_color, vec4 pulsed_sprite_color, vec4 border_color) {
    bool sprite_is_solid = base_color.a > SOLID_ALPHA_THRESHOLD;
    return sprite_is_solid ? pulsed_sprite_color : border_color;
}

void main() {
    // Base color from sprite.
    vec4 base_color = texture(atlas_texture, uv);
    // Pulsed interior color for the sprite.
    vec4 pulsed_sprite_color = sample_pulsed_sprite_color(base_color);
    // Color from border.
    vec4 border_color = sample_border_highlight(uv, base_color);
    frag_color = combine_passes(base_color, pulsed_sprite_color, border_color);
}
