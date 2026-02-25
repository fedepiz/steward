
#version 330
precision lowp float;

in vec2 uv;
out vec4 frag_color;

// Unused
uniform sampler2D Texture;
uniform sampler2D base_tex;
uniform sampler2D mask_tex;
uniform sampler2D overlay_atlas;
uniform float time;
uniform vec4 atlas_region;

float wave_value(vec2 uv) {
    float w = 2.0;
    float length = 5.0; //10.0;
    float shift_size = 5.0;
    float shift_time = 0.5;
    float shift = sin(uv.x * shift_size + time  * shift_time) * 0.75;

    float wave = sin((uv.y + shift) * length + time * w);
    return (wave + 1.0) / 2.0;
}

vec2 perturb(vec2 uv) {
    float wave = wave_value(uv);
    float min_sample_intensity = 0.025;
    float sample_scale = 1.0 - wave * min_sample_intensity;
    float sample_y = sample_scale * uv.y;
    return vec2(uv.x, sample_y);
}

void main() {
    float wave = wave_value(uv);
    vec2 uv_warp = perturb(uv);

    vec4 base_color = texture(base_tex, uv);
    vec4 mask_color = texture(mask_tex, uv_warp);

    vec4 color = vec4(0.0, 0.0, 0.0, 0.0);
    if (mask_color.r == 1) {
        // Attenuation (darken) based on wave, like the old shader.
        float min_intensity = 0.8; //0.7;
        float attenuation = wave * (1.0 - min_intensity);
        vec2 atlas_uv = atlas_region.xy + uv_warp * atlas_region.zw;
        vec4 tint = texture(overlay_atlas, atlas_uv);
        vec3 darkened = mix(tint.rgb, vec3(0.0, 0.0, 0.0), attenuation);
        color = vec4(darkened, tint.a);
    } else if (mask_color.r > 0) {
        color = vec4(0,0,0,1);
    } else if (mask_color.r == 0) {
        color = base_color;
    }

    frag_color = color;
}
