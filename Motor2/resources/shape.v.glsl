attribute vec2 coord2d;

uniform mat4 m_transform;

void main(void) {
  gl_Position = m_transform * vec4(coord2d, 0.0, 1.0);
}
