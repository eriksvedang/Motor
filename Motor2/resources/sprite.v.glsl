#version 110

attribute vec2 coord2d;
varying vec2 texcoord;

uniform mat4 m_transform;
uniform mat4 m_scale;
uniform mat4 m_cam;

void main(void) {
  gl_Position = m_cam * m_scale * m_transform * vec4(coord2d, 0.0, 1.0);
  texcoord = vec2(coord2d.x, -coord2d.y) * vec2(0.5) + vec2(0.5);
}
