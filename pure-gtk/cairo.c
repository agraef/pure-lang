#include <cairo/cairo.h>

int Pure_cairo_version()
{
  return cairo_version();
}

char const* Pure_cairo_version_string()
{
  return cairo_version_string();
}

cairo_t* Pure_cairo_create(cairo_surface_t* arg0)
{
  return cairo_create(arg0);
}

cairo_t* Pure_cairo_reference(cairo_t* arg0)
{
  return cairo_reference(arg0);
}

void Pure_cairo_destroy(cairo_t* arg0)
{
  return cairo_destroy(arg0);
}

unsigned int Pure_cairo_get_reference_count(cairo_t* arg0)
{
  return cairo_get_reference_count(arg0);
}

void* Pure_cairo_get_user_data(cairo_t* arg0, cairo_user_data_key_t const* arg1)
{
  return cairo_get_user_data(arg0, arg1);
}

unsigned int Pure_cairo_set_user_data(cairo_t* arg0, cairo_user_data_key_t const* arg1, void* arg2, void* arg3)
{
  return cairo_set_user_data(arg0, arg1, arg2, arg3);
}

void Pure_cairo_save(cairo_t* arg0)
{
  return cairo_save(arg0);
}

void Pure_cairo_restore(cairo_t* arg0)
{
  return cairo_restore(arg0);
}

void Pure_cairo_push_group(cairo_t* arg0)
{
  return cairo_push_group(arg0);
}

void Pure_cairo_push_group_with_content(cairo_t* arg0, unsigned int arg1)
{
  return cairo_push_group_with_content(arg0, arg1);
}

cairo_pattern_t* Pure_cairo_pop_group(cairo_t* arg0)
{
  return cairo_pop_group(arg0);
}

void Pure_cairo_pop_group_to_source(cairo_t* arg0)
{
  return cairo_pop_group_to_source(arg0);
}

void Pure_cairo_set_operator(cairo_t* arg0, unsigned int arg1)
{
  return cairo_set_operator(arg0, arg1);
}

void Pure_cairo_set_source(cairo_t* arg0, cairo_pattern_t* arg1)
{
  return cairo_set_source(arg0, arg1);
}

void Pure_cairo_set_source_rgb(cairo_t* arg0, double arg1, double arg2, double arg3)
{
  return cairo_set_source_rgb(arg0, arg1, arg2, arg3);
}

void Pure_cairo_set_source_rgba(cairo_t* arg0, double arg1, double arg2, double arg3, double arg4)
{
  return cairo_set_source_rgba(arg0, arg1, arg2, arg3, arg4);
}

void Pure_cairo_set_source_surface(cairo_t* arg0, cairo_surface_t* arg1, double arg2, double arg3)
{
  return cairo_set_source_surface(arg0, arg1, arg2, arg3);
}

void Pure_cairo_set_tolerance(cairo_t* arg0, double arg1)
{
  return cairo_set_tolerance(arg0, arg1);
}

void Pure_cairo_set_antialias(cairo_t* arg0, unsigned int arg1)
{
  return cairo_set_antialias(arg0, arg1);
}

void Pure_cairo_set_fill_rule(cairo_t* arg0, unsigned int arg1)
{
  return cairo_set_fill_rule(arg0, arg1);
}

void Pure_cairo_set_line_width(cairo_t* arg0, double arg1)
{
  return cairo_set_line_width(arg0, arg1);
}

void Pure_cairo_set_line_cap(cairo_t* arg0, unsigned int arg1)
{
  return cairo_set_line_cap(arg0, arg1);
}

void Pure_cairo_set_line_join(cairo_t* arg0, unsigned int arg1)
{
  return cairo_set_line_join(arg0, arg1);
}

void Pure_cairo_set_dash(cairo_t* arg0, double const* arg1, int arg2, double arg3)
{
  return cairo_set_dash(arg0, arg1, arg2, arg3);
}

void Pure_cairo_set_miter_limit(cairo_t* arg0, double arg1)
{
  return cairo_set_miter_limit(arg0, arg1);
}

void Pure_cairo_translate(cairo_t* arg0, double arg1, double arg2)
{
  return cairo_translate(arg0, arg1, arg2);
}

void Pure_cairo_scale(cairo_t* arg0, double arg1, double arg2)
{
  return cairo_scale(arg0, arg1, arg2);
}

void Pure_cairo_rotate(cairo_t* arg0, double arg1)
{
  return cairo_rotate(arg0, arg1);
}

void Pure_cairo_transform(cairo_t* arg0, cairo_matrix_t const* arg1)
{
  return cairo_transform(arg0, arg1);
}

void Pure_cairo_set_matrix(cairo_t* arg0, cairo_matrix_t const* arg1)
{
  return cairo_set_matrix(arg0, arg1);
}

void Pure_cairo_identity_matrix(cairo_t* arg0)
{
  return cairo_identity_matrix(arg0);
}

void Pure_cairo_user_to_device(cairo_t* arg0, double* arg1, double* arg2)
{
  return cairo_user_to_device(arg0, arg1, arg2);
}

void Pure_cairo_user_to_device_distance(cairo_t* arg0, double* arg1, double* arg2)
{
  return cairo_user_to_device_distance(arg0, arg1, arg2);
}

void Pure_cairo_device_to_user(cairo_t* arg0, double* arg1, double* arg2)
{
  return cairo_device_to_user(arg0, arg1, arg2);
}

void Pure_cairo_device_to_user_distance(cairo_t* arg0, double* arg1, double* arg2)
{
  return cairo_device_to_user_distance(arg0, arg1, arg2);
}

void Pure_cairo_new_path(cairo_t* arg0)
{
  return cairo_new_path(arg0);
}

void Pure_cairo_move_to(cairo_t* arg0, double arg1, double arg2)
{
  return cairo_move_to(arg0, arg1, arg2);
}

void Pure_cairo_new_sub_path(cairo_t* arg0)
{
  return cairo_new_sub_path(arg0);
}

void Pure_cairo_line_to(cairo_t* arg0, double arg1, double arg2)
{
  return cairo_line_to(arg0, arg1, arg2);
}

void Pure_cairo_curve_to(cairo_t* arg0, double arg1, double arg2, double arg3, double arg4, double arg5, double arg6)
{
  return cairo_curve_to(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_cairo_arc(cairo_t* arg0, double arg1, double arg2, double arg3, double arg4, double arg5)
{
  return cairo_arc(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_cairo_arc_negative(cairo_t* arg0, double arg1, double arg2, double arg3, double arg4, double arg5)
{
  return cairo_arc_negative(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_cairo_rel_move_to(cairo_t* arg0, double arg1, double arg2)
{
  return cairo_rel_move_to(arg0, arg1, arg2);
}

void Pure_cairo_rel_line_to(cairo_t* arg0, double arg1, double arg2)
{
  return cairo_rel_line_to(arg0, arg1, arg2);
}

void Pure_cairo_rel_curve_to(cairo_t* arg0, double arg1, double arg2, double arg3, double arg4, double arg5, double arg6)
{
  return cairo_rel_curve_to(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_cairo_rectangle(cairo_t* arg0, double arg1, double arg2, double arg3, double arg4)
{
  return cairo_rectangle(arg0, arg1, arg2, arg3, arg4);
}

void Pure_cairo_close_path(cairo_t* arg0)
{
  return cairo_close_path(arg0);
}

void Pure_cairo_path_extents(cairo_t* arg0, double* arg1, double* arg2, double* arg3, double* arg4)
{
  return cairo_path_extents(arg0, arg1, arg2, arg3, arg4);
}

void Pure_cairo_paint(cairo_t* arg0)
{
  return cairo_paint(arg0);
}

void Pure_cairo_paint_with_alpha(cairo_t* arg0, double arg1)
{
  return cairo_paint_with_alpha(arg0, arg1);
}

void Pure_cairo_mask(cairo_t* arg0, cairo_pattern_t* arg1)
{
  return cairo_mask(arg0, arg1);
}

void Pure_cairo_mask_surface(cairo_t* arg0, cairo_surface_t* arg1, double arg2, double arg3)
{
  return cairo_mask_surface(arg0, arg1, arg2, arg3);
}

void Pure_cairo_stroke(cairo_t* arg0)
{
  return cairo_stroke(arg0);
}

void Pure_cairo_stroke_preserve(cairo_t* arg0)
{
  return cairo_stroke_preserve(arg0);
}

void Pure_cairo_fill(cairo_t* arg0)
{
  return cairo_fill(arg0);
}

void Pure_cairo_fill_preserve(cairo_t* arg0)
{
  return cairo_fill_preserve(arg0);
}

void Pure_cairo_copy_page(cairo_t* arg0)
{
  return cairo_copy_page(arg0);
}

void Pure_cairo_show_page(cairo_t* arg0)
{
  return cairo_show_page(arg0);
}

int Pure_cairo_in_stroke(cairo_t* arg0, double arg1, double arg2)
{
  return cairo_in_stroke(arg0, arg1, arg2);
}

int Pure_cairo_in_fill(cairo_t* arg0, double arg1, double arg2)
{
  return cairo_in_fill(arg0, arg1, arg2);
}

void Pure_cairo_stroke_extents(cairo_t* arg0, double* arg1, double* arg2, double* arg3, double* arg4)
{
  return cairo_stroke_extents(arg0, arg1, arg2, arg3, arg4);
}

void Pure_cairo_fill_extents(cairo_t* arg0, double* arg1, double* arg2, double* arg3, double* arg4)
{
  return cairo_fill_extents(arg0, arg1, arg2, arg3, arg4);
}

void Pure_cairo_reset_clip(cairo_t* arg0)
{
  return cairo_reset_clip(arg0);
}

void Pure_cairo_clip(cairo_t* arg0)
{
  return cairo_clip(arg0);
}

void Pure_cairo_clip_preserve(cairo_t* arg0)
{
  return cairo_clip_preserve(arg0);
}

void Pure_cairo_clip_extents(cairo_t* arg0, double* arg1, double* arg2, double* arg3, double* arg4)
{
  return cairo_clip_extents(arg0, arg1, arg2, arg3, arg4);
}

cairo_rectangle_list_t* Pure_cairo_copy_clip_rectangle_list(cairo_t* arg0)
{
  return cairo_copy_clip_rectangle_list(arg0);
}

void Pure_cairo_rectangle_list_destroy(cairo_rectangle_list_t* arg0)
{
  return cairo_rectangle_list_destroy(arg0);
}

cairo_glyph_t* Pure_cairo_glyph_allocate(int arg0)
{
  return cairo_glyph_allocate(arg0);
}

void Pure_cairo_glyph_free(cairo_glyph_t* arg0)
{
  return cairo_glyph_free(arg0);
}

cairo_text_cluster_t* Pure_cairo_text_cluster_allocate(int arg0)
{
  return cairo_text_cluster_allocate(arg0);
}

void Pure_cairo_text_cluster_free(cairo_text_cluster_t* arg0)
{
  return cairo_text_cluster_free(arg0);
}

cairo_font_options_t* Pure_cairo_font_options_create()
{
  return cairo_font_options_create();
}

cairo_font_options_t* Pure_cairo_font_options_copy(cairo_font_options_t const* arg0)
{
  return cairo_font_options_copy(arg0);
}

void Pure_cairo_font_options_destroy(cairo_font_options_t* arg0)
{
  return cairo_font_options_destroy(arg0);
}

unsigned int Pure_cairo_font_options_status(cairo_font_options_t* arg0)
{
  return cairo_font_options_status(arg0);
}

void Pure_cairo_font_options_merge(cairo_font_options_t* arg0, cairo_font_options_t const* arg1)
{
  return cairo_font_options_merge(arg0, arg1);
}

int Pure_cairo_font_options_equal(cairo_font_options_t const* arg0, cairo_font_options_t const* arg1)
{
  return cairo_font_options_equal(arg0, arg1);
}

unsigned long Pure_cairo_font_options_hash(cairo_font_options_t const* arg0)
{
  return cairo_font_options_hash(arg0);
}

void Pure_cairo_font_options_set_antialias(cairo_font_options_t* arg0, unsigned int arg1)
{
  return cairo_font_options_set_antialias(arg0, arg1);
}

unsigned int Pure_cairo_font_options_get_antialias(cairo_font_options_t const* arg0)
{
  return cairo_font_options_get_antialias(arg0);
}

void Pure_cairo_font_options_set_subpixel_order(cairo_font_options_t* arg0, unsigned int arg1)
{
  return cairo_font_options_set_subpixel_order(arg0, arg1);
}

unsigned int Pure_cairo_font_options_get_subpixel_order(cairo_font_options_t const* arg0)
{
  return cairo_font_options_get_subpixel_order(arg0);
}

void Pure_cairo_font_options_set_hint_style(cairo_font_options_t* arg0, unsigned int arg1)
{
  return cairo_font_options_set_hint_style(arg0, arg1);
}

unsigned int Pure_cairo_font_options_get_hint_style(cairo_font_options_t const* arg0)
{
  return cairo_font_options_get_hint_style(arg0);
}

void Pure_cairo_font_options_set_hint_metrics(cairo_font_options_t* arg0, unsigned int arg1)
{
  return cairo_font_options_set_hint_metrics(arg0, arg1);
}

unsigned int Pure_cairo_font_options_get_hint_metrics(cairo_font_options_t const* arg0)
{
  return cairo_font_options_get_hint_metrics(arg0);
}

void Pure_cairo_select_font_face(cairo_t* arg0, char const* arg1, unsigned int arg2, unsigned int arg3)
{
  return cairo_select_font_face(arg0, arg1, arg2, arg3);
}

void Pure_cairo_set_font_size(cairo_t* arg0, double arg1)
{
  return cairo_set_font_size(arg0, arg1);
}

void Pure_cairo_set_font_matrix(cairo_t* arg0, cairo_matrix_t const* arg1)
{
  return cairo_set_font_matrix(arg0, arg1);
}

void Pure_cairo_get_font_matrix(cairo_t* arg0, cairo_matrix_t* arg1)
{
  return cairo_get_font_matrix(arg0, arg1);
}

void Pure_cairo_set_font_options(cairo_t* arg0, cairo_font_options_t const* arg1)
{
  return cairo_set_font_options(arg0, arg1);
}

void Pure_cairo_get_font_options(cairo_t* arg0, cairo_font_options_t* arg1)
{
  return cairo_get_font_options(arg0, arg1);
}

void Pure_cairo_set_font_face(cairo_t* arg0, cairo_font_face_t* arg1)
{
  return cairo_set_font_face(arg0, arg1);
}

cairo_font_face_t* Pure_cairo_get_font_face(cairo_t* arg0)
{
  return cairo_get_font_face(arg0);
}

void Pure_cairo_set_scaled_font(cairo_t* arg0, cairo_scaled_font_t const* arg1)
{
  return cairo_set_scaled_font(arg0, arg1);
}

cairo_scaled_font_t* Pure_cairo_get_scaled_font(cairo_t* arg0)
{
  return cairo_get_scaled_font(arg0);
}

void Pure_cairo_show_text(cairo_t* arg0, char const* arg1)
{
  return cairo_show_text(arg0, arg1);
}

void Pure_cairo_show_glyphs(cairo_t* arg0, cairo_glyph_t const* arg1, int arg2)
{
  return cairo_show_glyphs(arg0, arg1, arg2);
}

void Pure_cairo_show_text_glyphs(cairo_t* arg0, char const* arg1, int arg2, cairo_glyph_t const* arg3, int arg4, cairo_text_cluster_t const* arg5, int arg6, unsigned int arg7)
{
  return cairo_show_text_glyphs(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_cairo_text_path(cairo_t* arg0, char const* arg1)
{
  return cairo_text_path(arg0, arg1);
}

void Pure_cairo_glyph_path(cairo_t* arg0, cairo_glyph_t const* arg1, int arg2)
{
  return cairo_glyph_path(arg0, arg1, arg2);
}

void Pure_cairo_text_extents(cairo_t* arg0, char const* arg1, cairo_text_extents_t* arg2)
{
  return cairo_text_extents(arg0, arg1, arg2);
}

void Pure_cairo_glyph_extents(cairo_t* arg0, cairo_glyph_t const* arg1, int arg2, cairo_text_extents_t* arg3)
{
  return cairo_glyph_extents(arg0, arg1, arg2, arg3);
}

void Pure_cairo_font_extents(cairo_t* arg0, cairo_font_extents_t* arg1)
{
  return cairo_font_extents(arg0, arg1);
}

cairo_font_face_t* Pure_cairo_font_face_reference(cairo_font_face_t* arg0)
{
  return cairo_font_face_reference(arg0);
}

void Pure_cairo_font_face_destroy(cairo_font_face_t* arg0)
{
  return cairo_font_face_destroy(arg0);
}

unsigned int Pure_cairo_font_face_get_reference_count(cairo_font_face_t* arg0)
{
  return cairo_font_face_get_reference_count(arg0);
}

unsigned int Pure_cairo_font_face_status(cairo_font_face_t* arg0)
{
  return cairo_font_face_status(arg0);
}

unsigned int Pure_cairo_font_face_get_type(cairo_font_face_t* arg0)
{
  return cairo_font_face_get_type(arg0);
}

void* Pure_cairo_font_face_get_user_data(cairo_font_face_t* arg0, cairo_user_data_key_t const* arg1)
{
  return cairo_font_face_get_user_data(arg0, arg1);
}

unsigned int Pure_cairo_font_face_set_user_data(cairo_font_face_t* arg0, cairo_user_data_key_t const* arg1, void* arg2, void* arg3)
{
  return cairo_font_face_set_user_data(arg0, arg1, arg2, arg3);
}

cairo_scaled_font_t* Pure_cairo_scaled_font_create(cairo_font_face_t* arg0, cairo_matrix_t const* arg1, cairo_matrix_t const* arg2, cairo_font_options_t const* arg3)
{
  return cairo_scaled_font_create(arg0, arg1, arg2, arg3);
}

cairo_scaled_font_t* Pure_cairo_scaled_font_reference(cairo_scaled_font_t* arg0)
{
  return cairo_scaled_font_reference(arg0);
}

void Pure_cairo_scaled_font_destroy(cairo_scaled_font_t* arg0)
{
  return cairo_scaled_font_destroy(arg0);
}

unsigned int Pure_cairo_scaled_font_get_reference_count(cairo_scaled_font_t* arg0)
{
  return cairo_scaled_font_get_reference_count(arg0);
}

unsigned int Pure_cairo_scaled_font_status(cairo_scaled_font_t* arg0)
{
  return cairo_scaled_font_status(arg0);
}

unsigned int Pure_cairo_scaled_font_get_type(cairo_scaled_font_t* arg0)
{
  return cairo_scaled_font_get_type(arg0);
}

void* Pure_cairo_scaled_font_get_user_data(cairo_scaled_font_t* arg0, cairo_user_data_key_t const* arg1)
{
  return cairo_scaled_font_get_user_data(arg0, arg1);
}

unsigned int Pure_cairo_scaled_font_set_user_data(cairo_scaled_font_t* arg0, cairo_user_data_key_t const* arg1, void* arg2, void* arg3)
{
  return cairo_scaled_font_set_user_data(arg0, arg1, arg2, arg3);
}

void Pure_cairo_scaled_font_extents(cairo_scaled_font_t* arg0, cairo_font_extents_t* arg1)
{
  return cairo_scaled_font_extents(arg0, arg1);
}

void Pure_cairo_scaled_font_text_extents(cairo_scaled_font_t* arg0, char const* arg1, cairo_text_extents_t* arg2)
{
  return cairo_scaled_font_text_extents(arg0, arg1, arg2);
}

void Pure_cairo_scaled_font_glyph_extents(cairo_scaled_font_t* arg0, cairo_glyph_t const* arg1, int arg2, cairo_text_extents_t* arg3)
{
  return cairo_scaled_font_glyph_extents(arg0, arg1, arg2, arg3);
}

unsigned int Pure_cairo_scaled_font_text_to_glyphs(cairo_scaled_font_t* arg0, double arg1, double arg2, char const* arg3, int arg4, cairo_glyph_t** arg5, int* arg6, cairo_text_cluster_t** arg7, int* arg8, unsigned int* arg9)
{
  return cairo_scaled_font_text_to_glyphs(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

cairo_font_face_t* Pure_cairo_scaled_font_get_font_face(cairo_scaled_font_t* arg0)
{
  return cairo_scaled_font_get_font_face(arg0);
}

void Pure_cairo_scaled_font_get_font_matrix(cairo_scaled_font_t* arg0, cairo_matrix_t* arg1)
{
  return cairo_scaled_font_get_font_matrix(arg0, arg1);
}

void Pure_cairo_scaled_font_get_ctm(cairo_scaled_font_t* arg0, cairo_matrix_t* arg1)
{
  return cairo_scaled_font_get_ctm(arg0, arg1);
}

void Pure_cairo_scaled_font_get_scale_matrix(cairo_scaled_font_t* arg0, cairo_matrix_t* arg1)
{
  return cairo_scaled_font_get_scale_matrix(arg0, arg1);
}

void Pure_cairo_scaled_font_get_font_options(cairo_scaled_font_t* arg0, cairo_font_options_t* arg1)
{
  return cairo_scaled_font_get_font_options(arg0, arg1);
}

cairo_font_face_t* Pure_cairo_toy_font_face_create(char const* arg0, unsigned int arg1, unsigned int arg2)
{
  return cairo_toy_font_face_create(arg0, arg1, arg2);
}

char const* Pure_cairo_toy_font_face_get_family(cairo_font_face_t* arg0)
{
  return cairo_toy_font_face_get_family(arg0);
}

unsigned int Pure_cairo_toy_font_face_get_slant(cairo_font_face_t* arg0)
{
  return cairo_toy_font_face_get_slant(arg0);
}

unsigned int Pure_cairo_toy_font_face_get_weight(cairo_font_face_t* arg0)
{
  return cairo_toy_font_face_get_weight(arg0);
}

cairo_font_face_t* Pure_cairo_user_font_face_create()
{
  return cairo_user_font_face_create();
}

void Pure_cairo_user_font_face_set_init_func(cairo_font_face_t* arg0, void* arg1)
{
  return cairo_user_font_face_set_init_func(arg0, arg1);
}

void Pure_cairo_user_font_face_set_render_glyph_func(cairo_font_face_t* arg0, void* arg1)
{
  return cairo_user_font_face_set_render_glyph_func(arg0, arg1);
}

void Pure_cairo_user_font_face_set_text_to_glyphs_func(cairo_font_face_t* arg0, void* arg1)
{
  return cairo_user_font_face_set_text_to_glyphs_func(arg0, arg1);
}

void Pure_cairo_user_font_face_set_unicode_to_glyph_func(cairo_font_face_t* arg0, void* arg1)
{
  return cairo_user_font_face_set_unicode_to_glyph_func(arg0, arg1);
}

void* Pure_cairo_user_font_face_get_init_func(cairo_font_face_t* arg0)
{
  return cairo_user_font_face_get_init_func(arg0);
}

void* Pure_cairo_user_font_face_get_render_glyph_func(cairo_font_face_t* arg0)
{
  return cairo_user_font_face_get_render_glyph_func(arg0);
}

void* Pure_cairo_user_font_face_get_text_to_glyphs_func(cairo_font_face_t* arg0)
{
  return cairo_user_font_face_get_text_to_glyphs_func(arg0);
}

void* Pure_cairo_user_font_face_get_unicode_to_glyph_func(cairo_font_face_t* arg0)
{
  return cairo_user_font_face_get_unicode_to_glyph_func(arg0);
}

unsigned int Pure_cairo_get_operator(cairo_t* arg0)
{
  return cairo_get_operator(arg0);
}

cairo_pattern_t* Pure_cairo_get_source(cairo_t* arg0)
{
  return cairo_get_source(arg0);
}

double Pure_cairo_get_tolerance(cairo_t* arg0)
{
  return cairo_get_tolerance(arg0);
}

unsigned int Pure_cairo_get_antialias(cairo_t* arg0)
{
  return cairo_get_antialias(arg0);
}

int Pure_cairo_has_current_point(cairo_t* arg0)
{
  return cairo_has_current_point(arg0);
}

void Pure_cairo_get_current_point(cairo_t* arg0, double* arg1, double* arg2)
{
  return cairo_get_current_point(arg0, arg1, arg2);
}

unsigned int Pure_cairo_get_fill_rule(cairo_t* arg0)
{
  return cairo_get_fill_rule(arg0);
}

double Pure_cairo_get_line_width(cairo_t* arg0)
{
  return cairo_get_line_width(arg0);
}

unsigned int Pure_cairo_get_line_cap(cairo_t* arg0)
{
  return cairo_get_line_cap(arg0);
}

unsigned int Pure_cairo_get_line_join(cairo_t* arg0)
{
  return cairo_get_line_join(arg0);
}

double Pure_cairo_get_miter_limit(cairo_t* arg0)
{
  return cairo_get_miter_limit(arg0);
}

int Pure_cairo_get_dash_count(cairo_t* arg0)
{
  return cairo_get_dash_count(arg0);
}

void Pure_cairo_get_dash(cairo_t* arg0, double* arg1, double* arg2)
{
  return cairo_get_dash(arg0, arg1, arg2);
}

void Pure_cairo_get_matrix(cairo_t* arg0, cairo_matrix_t* arg1)
{
  return cairo_get_matrix(arg0, arg1);
}

cairo_surface_t* Pure_cairo_get_target(cairo_t* arg0)
{
  return cairo_get_target(arg0);
}

cairo_surface_t* Pure_cairo_get_group_target(cairo_t* arg0)
{
  return cairo_get_group_target(arg0);
}

cairo_path_t* Pure_cairo_copy_path(cairo_t* arg0)
{
  return cairo_copy_path(arg0);
}

cairo_path_t* Pure_cairo_copy_path_flat(cairo_t* arg0)
{
  return cairo_copy_path_flat(arg0);
}

void Pure_cairo_append_path(cairo_t* arg0, cairo_path_t const* arg1)
{
  return cairo_append_path(arg0, arg1);
}

void Pure_cairo_path_destroy(cairo_path_t* arg0)
{
  return cairo_path_destroy(arg0);
}

unsigned int Pure_cairo_status(cairo_t* arg0)
{
  return cairo_status(arg0);
}

char const* Pure_cairo_status_to_string(unsigned int arg0)
{
  return cairo_status_to_string(arg0);
}

cairo_surface_t* Pure_cairo_surface_create_similar(cairo_surface_t* arg0, unsigned int arg1, int arg2, int arg3)
{
  return cairo_surface_create_similar(arg0, arg1, arg2, arg3);
}

cairo_surface_t* Pure_cairo_surface_reference(cairo_surface_t* arg0)
{
  return cairo_surface_reference(arg0);
}

void Pure_cairo_surface_finish(cairo_surface_t* arg0)
{
  return cairo_surface_finish(arg0);
}

void Pure_cairo_surface_destroy(cairo_surface_t* arg0)
{
  return cairo_surface_destroy(arg0);
}

unsigned int Pure_cairo_surface_get_reference_count(cairo_surface_t* arg0)
{
  return cairo_surface_get_reference_count(arg0);
}

unsigned int Pure_cairo_surface_status(cairo_surface_t* arg0)
{
  return cairo_surface_status(arg0);
}

unsigned int Pure_cairo_surface_get_type(cairo_surface_t* arg0)
{
  return cairo_surface_get_type(arg0);
}

unsigned int Pure_cairo_surface_get_content(cairo_surface_t* arg0)
{
  return cairo_surface_get_content(arg0);
}

unsigned int Pure_cairo_surface_write_to_png(cairo_surface_t* arg0, char const* arg1)
{
  return cairo_surface_write_to_png(arg0, arg1);
}

unsigned int Pure_cairo_surface_write_to_png_stream(cairo_surface_t* arg0, void* arg1, void* arg2)
{
  return cairo_surface_write_to_png_stream(arg0, arg1, arg2);
}

void* Pure_cairo_surface_get_user_data(cairo_surface_t* arg0, cairo_user_data_key_t const* arg1)
{
  return cairo_surface_get_user_data(arg0, arg1);
}

unsigned int Pure_cairo_surface_set_user_data(cairo_surface_t* arg0, cairo_user_data_key_t const* arg1, void* arg2, void* arg3)
{
  return cairo_surface_set_user_data(arg0, arg1, arg2, arg3);
}

void Pure_cairo_surface_get_font_options(cairo_surface_t* arg0, cairo_font_options_t* arg1)
{
  return cairo_surface_get_font_options(arg0, arg1);
}

void Pure_cairo_surface_flush(cairo_surface_t* arg0)
{
  return cairo_surface_flush(arg0);
}

void Pure_cairo_surface_mark_dirty(cairo_surface_t* arg0)
{
  return cairo_surface_mark_dirty(arg0);
}

void Pure_cairo_surface_mark_dirty_rectangle(cairo_surface_t* arg0, int arg1, int arg2, int arg3, int arg4)
{
  return cairo_surface_mark_dirty_rectangle(arg0, arg1, arg2, arg3, arg4);
}

void Pure_cairo_surface_set_device_offset(cairo_surface_t* arg0, double arg1, double arg2)
{
  return cairo_surface_set_device_offset(arg0, arg1, arg2);
}

void Pure_cairo_surface_get_device_offset(cairo_surface_t* arg0, double* arg1, double* arg2)
{
  return cairo_surface_get_device_offset(arg0, arg1, arg2);
}

void Pure_cairo_surface_set_fallback_resolution(cairo_surface_t* arg0, double arg1, double arg2)
{
  return cairo_surface_set_fallback_resolution(arg0, arg1, arg2);
}

void Pure_cairo_surface_get_fallback_resolution(cairo_surface_t* arg0, double* arg1, double* arg2)
{
  return cairo_surface_get_fallback_resolution(arg0, arg1, arg2);
}

void Pure_cairo_surface_copy_page(cairo_surface_t* arg0)
{
  return cairo_surface_copy_page(arg0);
}

void Pure_cairo_surface_show_page(cairo_surface_t* arg0)
{
  return cairo_surface_show_page(arg0);
}

int Pure_cairo_surface_has_show_text_glyphs(cairo_surface_t* arg0)
{
  return cairo_surface_has_show_text_glyphs(arg0);
}

cairo_surface_t* Pure_cairo_image_surface_create(unsigned int arg0, int arg1, int arg2)
{
  return cairo_image_surface_create(arg0, arg1, arg2);
}

int Pure_cairo_format_stride_for_width(unsigned int arg0, int arg1)
{
  return cairo_format_stride_for_width(arg0, arg1);
}

cairo_surface_t* Pure_cairo_image_surface_create_for_data(unsigned char* arg0, unsigned int arg1, int arg2, int arg3, int arg4)
{
  return cairo_image_surface_create_for_data(arg0, arg1, arg2, arg3, arg4);
}

unsigned char* Pure_cairo_image_surface_get_data(cairo_surface_t* arg0)
{
  return cairo_image_surface_get_data(arg0);
}

unsigned int Pure_cairo_image_surface_get_format(cairo_surface_t* arg0)
{
  return cairo_image_surface_get_format(arg0);
}

int Pure_cairo_image_surface_get_width(cairo_surface_t* arg0)
{
  return cairo_image_surface_get_width(arg0);
}

int Pure_cairo_image_surface_get_height(cairo_surface_t* arg0)
{
  return cairo_image_surface_get_height(arg0);
}

int Pure_cairo_image_surface_get_stride(cairo_surface_t* arg0)
{
  return cairo_image_surface_get_stride(arg0);
}

cairo_surface_t* Pure_cairo_image_surface_create_from_png(char const* arg0)
{
  return cairo_image_surface_create_from_png(arg0);
}

cairo_surface_t* Pure_cairo_image_surface_create_from_png_stream(void* arg0, void* arg1)
{
  return cairo_image_surface_create_from_png_stream(arg0, arg1);
}

cairo_pattern_t* Pure_cairo_pattern_create_rgb(double arg0, double arg1, double arg2)
{
  return cairo_pattern_create_rgb(arg0, arg1, arg2);
}

cairo_pattern_t* Pure_cairo_pattern_create_rgba(double arg0, double arg1, double arg2, double arg3)
{
  return cairo_pattern_create_rgba(arg0, arg1, arg2, arg3);
}

cairo_pattern_t* Pure_cairo_pattern_create_for_surface(cairo_surface_t* arg0)
{
  return cairo_pattern_create_for_surface(arg0);
}

cairo_pattern_t* Pure_cairo_pattern_create_linear(double arg0, double arg1, double arg2, double arg3)
{
  return cairo_pattern_create_linear(arg0, arg1, arg2, arg3);
}

cairo_pattern_t* Pure_cairo_pattern_create_radial(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5)
{
  return cairo_pattern_create_radial(arg0, arg1, arg2, arg3, arg4, arg5);
}

cairo_pattern_t* Pure_cairo_pattern_reference(cairo_pattern_t* arg0)
{
  return cairo_pattern_reference(arg0);
}

void Pure_cairo_pattern_destroy(cairo_pattern_t* arg0)
{
  return cairo_pattern_destroy(arg0);
}

unsigned int Pure_cairo_pattern_get_reference_count(cairo_pattern_t* arg0)
{
  return cairo_pattern_get_reference_count(arg0);
}

unsigned int Pure_cairo_pattern_status(cairo_pattern_t* arg0)
{
  return cairo_pattern_status(arg0);
}

void* Pure_cairo_pattern_get_user_data(cairo_pattern_t* arg0, cairo_user_data_key_t const* arg1)
{
  return cairo_pattern_get_user_data(arg0, arg1);
}

unsigned int Pure_cairo_pattern_set_user_data(cairo_pattern_t* arg0, cairo_user_data_key_t const* arg1, void* arg2, void* arg3)
{
  return cairo_pattern_set_user_data(arg0, arg1, arg2, arg3);
}

unsigned int Pure_cairo_pattern_get_type(cairo_pattern_t* arg0)
{
  return cairo_pattern_get_type(arg0);
}

void Pure_cairo_pattern_add_color_stop_rgb(cairo_pattern_t* arg0, double arg1, double arg2, double arg3, double arg4)
{
  return cairo_pattern_add_color_stop_rgb(arg0, arg1, arg2, arg3, arg4);
}

void Pure_cairo_pattern_add_color_stop_rgba(cairo_pattern_t* arg0, double arg1, double arg2, double arg3, double arg4, double arg5)
{
  return cairo_pattern_add_color_stop_rgba(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_cairo_pattern_set_matrix(cairo_pattern_t* arg0, cairo_matrix_t const* arg1)
{
  return cairo_pattern_set_matrix(arg0, arg1);
}

void Pure_cairo_pattern_get_matrix(cairo_pattern_t* arg0, cairo_matrix_t* arg1)
{
  return cairo_pattern_get_matrix(arg0, arg1);
}

void Pure_cairo_pattern_set_extend(cairo_pattern_t* arg0, unsigned int arg1)
{
  return cairo_pattern_set_extend(arg0, arg1);
}

unsigned int Pure_cairo_pattern_get_extend(cairo_pattern_t* arg0)
{
  return cairo_pattern_get_extend(arg0);
}

void Pure_cairo_pattern_set_filter(cairo_pattern_t* arg0, unsigned int arg1)
{
  return cairo_pattern_set_filter(arg0, arg1);
}

unsigned int Pure_cairo_pattern_get_filter(cairo_pattern_t* arg0)
{
  return cairo_pattern_get_filter(arg0);
}

unsigned int Pure_cairo_pattern_get_rgba(cairo_pattern_t* arg0, double* arg1, double* arg2, double* arg3, double* arg4)
{
  return cairo_pattern_get_rgba(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_cairo_pattern_get_surface(cairo_pattern_t* arg0, cairo_surface_t** arg1)
{
  return cairo_pattern_get_surface(arg0, arg1);
}

unsigned int Pure_cairo_pattern_get_color_stop_rgba(cairo_pattern_t* arg0, int arg1, double* arg2, double* arg3, double* arg4, double* arg5, double* arg6)
{
  return cairo_pattern_get_color_stop_rgba(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

unsigned int Pure_cairo_pattern_get_color_stop_count(cairo_pattern_t* arg0, int* arg1)
{
  return cairo_pattern_get_color_stop_count(arg0, arg1);
}

unsigned int Pure_cairo_pattern_get_linear_points(cairo_pattern_t* arg0, double* arg1, double* arg2, double* arg3, double* arg4)
{
  return cairo_pattern_get_linear_points(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_cairo_pattern_get_radial_circles(cairo_pattern_t* arg0, double* arg1, double* arg2, double* arg3, double* arg4, double* arg5, double* arg6)
{
  return cairo_pattern_get_radial_circles(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_cairo_matrix_init(cairo_matrix_t* arg0, double arg1, double arg2, double arg3, double arg4, double arg5, double arg6)
{
  return cairo_matrix_init(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_cairo_matrix_init_identity(cairo_matrix_t* arg0)
{
  return cairo_matrix_init_identity(arg0);
}

void Pure_cairo_matrix_init_translate(cairo_matrix_t* arg0, double arg1, double arg2)
{
  return cairo_matrix_init_translate(arg0, arg1, arg2);
}

void Pure_cairo_matrix_init_scale(cairo_matrix_t* arg0, double arg1, double arg2)
{
  return cairo_matrix_init_scale(arg0, arg1, arg2);
}

void Pure_cairo_matrix_init_rotate(cairo_matrix_t* arg0, double arg1)
{
  return cairo_matrix_init_rotate(arg0, arg1);
}

void Pure_cairo_matrix_translate(cairo_matrix_t* arg0, double arg1, double arg2)
{
  return cairo_matrix_translate(arg0, arg1, arg2);
}

void Pure_cairo_matrix_scale(cairo_matrix_t* arg0, double arg1, double arg2)
{
  return cairo_matrix_scale(arg0, arg1, arg2);
}

void Pure_cairo_matrix_rotate(cairo_matrix_t* arg0, double arg1)
{
  return cairo_matrix_rotate(arg0, arg1);
}

unsigned int Pure_cairo_matrix_invert(cairo_matrix_t* arg0)
{
  return cairo_matrix_invert(arg0);
}

void Pure_cairo_matrix_multiply(cairo_matrix_t* arg0, cairo_matrix_t const* arg1, cairo_matrix_t const* arg2)
{
  return cairo_matrix_multiply(arg0, arg1, arg2);
}

void Pure_cairo_matrix_transform_distance(cairo_matrix_t const* arg0, double* arg1, double* arg2)
{
  return cairo_matrix_transform_distance(arg0, arg1, arg2);
}

void Pure_cairo_matrix_transform_point(cairo_matrix_t const* arg0, double* arg1, double* arg2)
{
  return cairo_matrix_transform_point(arg0, arg1, arg2);
}

void Pure_cairo_debug_reset_static_data()
{
  return cairo_debug_reset_static_data();
}
