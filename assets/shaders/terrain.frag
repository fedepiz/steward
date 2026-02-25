#version 330
precision lowp float;

in vec2 uv;
out vec4 frag_color;


uniform sampler2D Texture;
uniform sampler2D terrain_key;
uniform sampler2D terrain_atlas;
// The actual scale, in pixels of the texture as shown on screen.
uniform vec2 pixel_scale;

vec4 sample_by_pixel(sampler2D tex, vec2 pos) {
    vec2 uv = pos/textureSize(tex,0);
    return texture(tex, uv);
}

vec4 sample_terrain_atlas(vec2 terrain_index) {
    // The size in x and y dimeansion of a tile in the terrain atlas
    const float terrain_tile_pixel_size = 256;
    // Scaling from "screen pixels" to "terrain atlas pixels".
    // This controls how quickly we repeat
    const float wrap_scale = 30.0;
    // Top left corner of the terrain tile (in px)
    vec2 tl_corner = terrain_index * terrain_tile_pixel_size;
    // The offset within the tile
    vec2 pixel_xy = uv * pixel_scale;
    vec2 offset = mod(pixel_xy * wrap_scale, terrain_tile_pixel_size);
    return sample_by_pixel(terrain_atlas, tl_corner + offset);
}

vec2 sample_terrain_key(vec2 uv) {
    vec4 color = texture(terrain_key, uv);
    return color.xy*255.;
}

void main() {
    // Find out the cell_ij in the logical grid of terrian cells
    vec2 key_size = vec2(textureSize(terrain_key,0));
    vec2 cell_ij = uv * key_size;
    
    // Integer cell coordinate and fractional position within the cell.
    vec2 cell = floor(cell_ij);
    vec2 frac = fract(cell_ij);

    // Clamp sampling to borders (use cell centers to avoid edge ambiguity).
    vec2 min_uv = vec2(0.5) / key_size;
    vec2 max_uv = (key_size - 0.5) / key_size;

    // Sample terrain keys at the centers of the 2x2 neighborhood.
    vec2 uv00 = clamp((cell + vec2(0.0, 0.0) + 0.5) / key_size, min_uv, max_uv);
    vec2 uv10 = clamp((cell + vec2(1.0, 0.0) + 0.5) / key_size, min_uv, max_uv);
    vec2 uv01 = clamp((cell + vec2(0.0, 1.0) + 0.5) / key_size, min_uv, max_uv);
    vec2 uv11 = clamp((cell + vec2(1.0, 1.0) + 0.5) / key_size, min_uv, max_uv);

    // Resolve each key to its atlas tile color.
    vec4 c00 = sample_terrain_atlas(sample_terrain_key(uv00));
    vec4 c10 = sample_terrain_atlas(sample_terrain_key(uv10));
    vec4 c01 = sample_terrain_atlas(sample_terrain_key(uv01));
    vec4 c11 = sample_terrain_atlas(sample_terrain_key(uv11));

    // Bilinear blend using the fractional position within the cell.
    vec4 cx0 = mix(c00, c10, frac.x);
    vec4 cx1 = mix(c01, c11, frac.x);
    frag_color = mix(cx0, cx1, frac.y);
}
