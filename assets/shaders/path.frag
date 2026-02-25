#version 330
precision lowp float;

in vec2 uv;
out vec4 frag_color;

uniform sampler2D tex;
uniform vec2 path_size;

void main() {
    vec2 tex_size = vec2(textureSize(tex, 0));
    // Repeat in X based on pixel length, with scale derived from Y stretch.
    float scale = path_size.y / max(tex_size.y, 0.0001);
    float u_px = uv.x * path_size.x / max(scale, 0.0001);
    float u = mod(u_px, tex_size.x) / tex_size.x;
    frag_color = texture(tex, vec2(u, uv.y));
}
