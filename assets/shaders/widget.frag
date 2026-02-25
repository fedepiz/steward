#version 330
precision lowp float;

in vec2 uv;
out vec4 frag_color;

uniform vec4 fill_color;
uniform vec4 stroke_color;
uniform float stroke_thickness;
uniform vec2 rect_size;
// corner_radius: radius in pixels. 0 disables rounding.
uniform float corner_radius;
// background_intensity: 0 disables background texture, 1 shows fully.
uniform float background_intensity;
uniform sampler2D background_tex;
// Atlas region in normalized UV space: (u_min, v_min, u_size, v_size).
uniform vec4 atlas_region;
// Pulse parameters for selection highlight.
// pulse_intensity: 0 disables pulse, 1 is full configured pulse.
// time: seconds, used to animate the pulse.
uniform float pulse_intensity;
uniform float time;
// Inner shadow parameters for inset/pressed look.
// shadow_strength: 0 disables shading, 1 is full shading.
// shadow_size: thickness in pixels of the shaded edge region.
uniform float shadow_strength;
uniform float shadow_size;

float edge_distance_px(vec2 at_uv, vec2 size) {
    float left = at_uv.x * size.x;
    float right = (1.0 - at_uv.x) * size.x;
    float top = at_uv.y * size.y;
    float bottom = (1.0 - at_uv.y) * size.y;
    return min(min(left, right), min(top, bottom));
}

float rounded_rect_alpha(vec2 at_uv, vec2 size, float radius) {
    if (radius <= 0.0) {
        return 1.0;
    }

    float r = min(radius, 0.5 * min(size.x, size.y));
    vec2 p = at_uv * size;
    vec2 inner_min = vec2(r, r);
    vec2 inner_max = size - vec2(r, r);
    vec2 clamped = clamp(p, inner_min, inner_max);
    float dist = length(p - clamped);
    // Smooth edge by 1px.
    float aa = 1.0;
    return 1.0 - smoothstep(r - aa, r + aa, dist);
}

// Apply an inner shadow that brightens top/left and darkens bottom/right.
vec4 apply_inner_shadow(vec4 base, vec2 at_uv, vec2 size) {
    if (shadow_strength <= 0.0 || shadow_size <= 0.0 || base.a <= 0.0) {
        return base;
    }

    float top = at_uv.y * size.y;
    float left = at_uv.x * size.x;
    float bottom = (1.0 - at_uv.y) * size.y;
    float right = (1.0 - at_uv.x) * size.x;

    float top_w = 1.0 - smoothstep(0.0, shadow_size, top);
    float left_w = 1.0 - smoothstep(0.0, shadow_size, left);
    float bottom_w = 1.0 - smoothstep(0.0, shadow_size, bottom);
    float right_w = 1.0 - smoothstep(0.0, shadow_size, right);

    float light_w = clamp(top_w + left_w, 0.0, 1.0);
    float dark_w = clamp(bottom_w + right_w, 0.0, 1.0);

    // Fixed 20% mix towards white/black, scaled by shadow_strength.
    vec3 light = mix(base.rgb, vec3(1.0), 0.2);
    vec3 dark = mix(base.rgb, vec3(0.0), 0.2);

    vec3 shaded = mix(base.rgb, light, light_w * shadow_strength);
    shaded = mix(shaded, dark, dark_w * shadow_strength);
    return vec4(shaded, base.a);
}

vec4 apply_pulse(vec4 base) {
    if (pulse_intensity <= 0.0 || base.a <= 0.0) {
        return base;
    }

    const float PULSE_SPEED = 4.5;
    const float MAX_PULSE_BRIGHTEN = 1.;

    float pulse_wave = 0.5 + 0.5 * sin(time * PULSE_SPEED);
    float pulse_amount = clamp(pulse_intensity, 0.0, 1.0) * pulse_wave * MAX_PULSE_BRIGHTEN;

    vec3 pulsed_rgb = mix(base.rgb, vec3(1.0), pulse_amount);
    return vec4(pulsed_rgb, base.a);
}

vec4 apply_background(vec4 base, vec2 at_uv, vec2 size) {
    if (background_intensity <= 0.0) {
        return base;
    }

    vec2 atlas_size = vec2(textureSize(background_tex, 0));
    vec2 region_size_uv = atlas_region.zw;
    if (region_size_uv.x <= 0.0 || region_size_uv.y <= 0.0) {
        return base;
    }

    vec2 region_size_px = region_size_uv * atlas_size;
    vec2 pixel = at_uv * size;
    vec2 tiled_region_uv = fract(pixel / region_size_px);
    vec2 sample_uv = atlas_region.xy + tiled_region_uv * region_size_uv;
    vec4 texel = texture(background_tex, sample_uv);
    return mix(base, texel, clamp(background_intensity, 0.0, 1.0));
}

void main() {
    float edge_px = edge_distance_px(uv, rect_size);
    bool draw_stroke = stroke_thickness > 0.0
        && stroke_color.a > 0.0
        && edge_px <= stroke_thickness;

    if (draw_stroke) {
        float alpha = rounded_rect_alpha(uv, rect_size, corner_radius);
        frag_color = vec4(stroke_color.rgb, stroke_color.a * alpha);
        return;
    }

    vec4 base = apply_background(fill_color, uv, rect_size);
    base = apply_inner_shadow(base, uv, rect_size);
    base = apply_pulse(base);
    float alpha = rounded_rect_alpha(uv, rect_size, corner_radius);
    frag_color = vec4(base.rgb, base.a * alpha);
}
