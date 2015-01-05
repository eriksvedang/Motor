#version 110

attribute vec2 v_coord;
varying vec2 texcoord;

uniform mat4 m_transform;

void main(void) {
  gl_Position = m_transform * vec4(v_coord, 0.0, 1.0);
  texcoord = vec2(v_coord.x, -v_coord.y) * vec2(0.5) + vec2(0.5);
}
