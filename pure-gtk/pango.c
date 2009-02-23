#include <pango/pango.h>

PangoCoverage* Pure_pango_coverage_new()
{
  return pango_coverage_new();
}

PangoCoverage* Pure_pango_coverage_ref(PangoCoverage* arg0)
{
  return pango_coverage_ref(arg0);
}

void Pure_pango_coverage_unref(PangoCoverage* arg0)
{
  return pango_coverage_unref(arg0);
}

PangoCoverage* Pure_pango_coverage_copy(PangoCoverage* arg0)
{
  return pango_coverage_copy(arg0);
}

unsigned int Pure_pango_coverage_get(PangoCoverage* arg0, int arg1)
{
  return pango_coverage_get(arg0, arg1);
}

void Pure_pango_coverage_set(PangoCoverage* arg0, int arg1, unsigned int arg2)
{
  return pango_coverage_set(arg0, arg1, arg2);
}

void Pure_pango_coverage_max(PangoCoverage* arg0, PangoCoverage* arg1)
{
  return pango_coverage_max(arg0, arg1);
}

void Pure_pango_coverage_to_bytes(PangoCoverage* arg0, unsigned char** arg1, int* arg2)
{
  return pango_coverage_to_bytes(arg0, arg1, arg2);
}

PangoCoverage* Pure_pango_coverage_from_bytes(unsigned char* arg0, int arg1)
{
  return pango_coverage_from_bytes(arg0, arg1);
}

int Pure_pango_units_from_double(double arg0)
{
  return pango_units_from_double(arg0);
}

double Pure_pango_units_to_double(int arg0)
{
  return pango_units_to_double(arg0);
}

void Pure_pango_extents_to_pixels(PangoRectangle* arg0, PangoRectangle* arg1)
{
  return pango_extents_to_pixels(arg0, arg1);
}

unsigned long Pure_pango_matrix_get_type()
{
  return pango_matrix_get_type();
}

PangoMatrix* Pure_pango_matrix_copy(PangoMatrix const* arg0)
{
  return pango_matrix_copy(arg0);
}

void Pure_pango_matrix_free(PangoMatrix* arg0)
{
  return pango_matrix_free(arg0);
}

void Pure_pango_matrix_translate(PangoMatrix* arg0, double arg1, double arg2)
{
  return pango_matrix_translate(arg0, arg1, arg2);
}

void Pure_pango_matrix_scale(PangoMatrix* arg0, double arg1, double arg2)
{
  return pango_matrix_scale(arg0, arg1, arg2);
}

void Pure_pango_matrix_rotate(PangoMatrix* arg0, double arg1)
{
  return pango_matrix_rotate(arg0, arg1);
}

void Pure_pango_matrix_concat(PangoMatrix* arg0, PangoMatrix const* arg1)
{
  return pango_matrix_concat(arg0, arg1);
}

void Pure_pango_matrix_transform_point(PangoMatrix const* arg0, double* arg1, double* arg2)
{
  return pango_matrix_transform_point(arg0, arg1, arg2);
}

void Pure_pango_matrix_transform_distance(PangoMatrix const* arg0, double* arg1, double* arg2)
{
  return pango_matrix_transform_distance(arg0, arg1, arg2);
}

void Pure_pango_matrix_transform_rectangle(PangoMatrix const* arg0, PangoRectangle* arg1)
{
  return pango_matrix_transform_rectangle(arg0, arg1);
}

void Pure_pango_matrix_transform_pixel_rectangle(PangoMatrix const* arg0, PangoRectangle* arg1)
{
  return pango_matrix_transform_pixel_rectangle(arg0, arg1);
}

double Pure_pango_matrix_get_font_scale_factor(PangoMatrix const* arg0)
{
  return pango_matrix_get_font_scale_factor(arg0);
}

int Pure_pango_script_for_unichar(unsigned int arg0)
{
  return pango_script_for_unichar(arg0);
}

PangoScriptIter* Pure_pango_script_iter_new(char const* arg0, int arg1)
{
  return pango_script_iter_new(arg0, arg1);
}

void Pure_pango_script_iter_get_range(PangoScriptIter* arg0, char const** arg1, char const** arg2, int* arg3)
{
  return pango_script_iter_get_range(arg0, arg1, arg2, arg3);
}

int Pure_pango_script_iter_next(PangoScriptIter* arg0)
{
  return pango_script_iter_next(arg0);
}

void Pure_pango_script_iter_free(PangoScriptIter* arg0)
{
  return pango_script_iter_free(arg0);
}

unsigned long Pure_pango_language_get_type()
{
  return pango_language_get_type();
}

PangoLanguage* Pure_pango_language_from_string(char const* arg0)
{
  return pango_language_from_string(arg0);
}

char const* Pure_pango_language_to_string(PangoLanguage* arg0)
{
  return pango_language_to_string(arg0);
}

char const* Pure_pango_language_get_sample_string(PangoLanguage* arg0)
{
  return pango_language_get_sample_string(arg0);
}

PangoLanguage* Pure_pango_language_get_default()
{
  return pango_language_get_default();
}

int Pure_pango_language_matches(PangoLanguage* arg0, char const* arg1)
{
  return pango_language_matches(arg0, arg1);
}

int Pure_pango_language_includes_script(PangoLanguage* arg0, int arg1)
{
  return pango_language_includes_script(arg0, arg1);
}

int const* Pure_pango_language_get_scripts(PangoLanguage* arg0, int* arg1)
{
  return pango_language_get_scripts(arg0, arg1);
}

PangoLanguage* Pure_pango_script_get_sample_language(int arg0)
{
  return pango_script_get_sample_language(arg0);
}

double Pure_pango_gravity_to_rotation(unsigned int arg0)
{
  return pango_gravity_to_rotation(arg0);
}

unsigned int Pure_pango_gravity_get_for_matrix(PangoMatrix const* arg0)
{
  return pango_gravity_get_for_matrix(arg0);
}

unsigned int Pure_pango_gravity_get_for_script(int arg0, unsigned int arg1, unsigned int arg2)
{
  return pango_gravity_get_for_script(arg0, arg1, arg2);
}

unsigned int Pure_pango_bidi_type_for_unichar(unsigned int arg0)
{
  return pango_bidi_type_for_unichar(arg0);
}

unsigned int Pure_pango_unichar_direction(unsigned int arg0)
{
  return pango_unichar_direction(arg0);
}

unsigned int Pure_pango_find_base_dir(char const* arg0, int arg1)
{
  return pango_find_base_dir(arg0, arg1);
}

int Pure_pango_get_mirror_char(unsigned int arg0, unsigned int* arg1)
{
  return pango_get_mirror_char(arg0, arg1);
}

unsigned long Pure_pango_font_description_get_type()
{
  return pango_font_description_get_type();
}

PangoFontDescription* Pure_pango_font_description_new()
{
  return pango_font_description_new();
}

PangoFontDescription* Pure_pango_font_description_copy(PangoFontDescription const* arg0)
{
  return pango_font_description_copy(arg0);
}

PangoFontDescription* Pure_pango_font_description_copy_static(PangoFontDescription const* arg0)
{
  return pango_font_description_copy_static(arg0);
}

unsigned int Pure_pango_font_description_hash(PangoFontDescription const* arg0)
{
  return pango_font_description_hash(arg0);
}

int Pure_pango_font_description_equal(PangoFontDescription const* arg0, PangoFontDescription const* arg1)
{
  return pango_font_description_equal(arg0, arg1);
}

void Pure_pango_font_description_free(PangoFontDescription* arg0)
{
  return pango_font_description_free(arg0);
}

void Pure_pango_font_descriptions_free(PangoFontDescription** arg0, int arg1)
{
  return pango_font_descriptions_free(arg0, arg1);
}

void Pure_pango_font_description_set_family(PangoFontDescription* arg0, char const* arg1)
{
  return pango_font_description_set_family(arg0, arg1);
}

void Pure_pango_font_description_set_family_static(PangoFontDescription* arg0, char const* arg1)
{
  return pango_font_description_set_family_static(arg0, arg1);
}

char const* Pure_pango_font_description_get_family(PangoFontDescription const* arg0)
{
  return pango_font_description_get_family(arg0);
}

void Pure_pango_font_description_set_style(PangoFontDescription* arg0, unsigned int arg1)
{
  return pango_font_description_set_style(arg0, arg1);
}

unsigned int Pure_pango_font_description_get_style(PangoFontDescription const* arg0)
{
  return pango_font_description_get_style(arg0);
}

void Pure_pango_font_description_set_variant(PangoFontDescription* arg0, unsigned int arg1)
{
  return pango_font_description_set_variant(arg0, arg1);
}

unsigned int Pure_pango_font_description_get_variant(PangoFontDescription const* arg0)
{
  return pango_font_description_get_variant(arg0);
}

void Pure_pango_font_description_set_weight(PangoFontDescription* arg0, unsigned int arg1)
{
  return pango_font_description_set_weight(arg0, arg1);
}

unsigned int Pure_pango_font_description_get_weight(PangoFontDescription const* arg0)
{
  return pango_font_description_get_weight(arg0);
}

void Pure_pango_font_description_set_stretch(PangoFontDescription* arg0, unsigned int arg1)
{
  return pango_font_description_set_stretch(arg0, arg1);
}

unsigned int Pure_pango_font_description_get_stretch(PangoFontDescription const* arg0)
{
  return pango_font_description_get_stretch(arg0);
}

void Pure_pango_font_description_set_size(PangoFontDescription* arg0, int arg1)
{
  return pango_font_description_set_size(arg0, arg1);
}

int Pure_pango_font_description_get_size(PangoFontDescription const* arg0)
{
  return pango_font_description_get_size(arg0);
}

void Pure_pango_font_description_set_absolute_size(PangoFontDescription* arg0, double arg1)
{
  return pango_font_description_set_absolute_size(arg0, arg1);
}

int Pure_pango_font_description_get_size_is_absolute(PangoFontDescription const* arg0)
{
  return pango_font_description_get_size_is_absolute(arg0);
}

void Pure_pango_font_description_set_gravity(PangoFontDescription* arg0, unsigned int arg1)
{
  return pango_font_description_set_gravity(arg0, arg1);
}

unsigned int Pure_pango_font_description_get_gravity(PangoFontDescription const* arg0)
{
  return pango_font_description_get_gravity(arg0);
}

unsigned int Pure_pango_font_description_get_set_fields(PangoFontDescription const* arg0)
{
  return pango_font_description_get_set_fields(arg0);
}

void Pure_pango_font_description_unset_fields(PangoFontDescription* arg0, unsigned int arg1)
{
  return pango_font_description_unset_fields(arg0, arg1);
}

void Pure_pango_font_description_merge(PangoFontDescription* arg0, PangoFontDescription const* arg1, int arg2)
{
  return pango_font_description_merge(arg0, arg1, arg2);
}

void Pure_pango_font_description_merge_static(PangoFontDescription* arg0, PangoFontDescription const* arg1, int arg2)
{
  return pango_font_description_merge_static(arg0, arg1, arg2);
}

int Pure_pango_font_description_better_match(PangoFontDescription const* arg0, PangoFontDescription const* arg1, PangoFontDescription const* arg2)
{
  return pango_font_description_better_match(arg0, arg1, arg2);
}

PangoFontDescription* Pure_pango_font_description_from_string(char const* arg0)
{
  return pango_font_description_from_string(arg0);
}

char* Pure_pango_font_description_to_string(PangoFontDescription const* arg0)
{
  return pango_font_description_to_string(arg0);
}

char* Pure_pango_font_description_to_filename(PangoFontDescription const* arg0)
{
  return pango_font_description_to_filename(arg0);
}

unsigned long Pure_pango_font_metrics_get_type()
{
  return pango_font_metrics_get_type();
}

PangoFontMetrics* Pure_pango_font_metrics_ref(PangoFontMetrics* arg0)
{
  return pango_font_metrics_ref(arg0);
}

void Pure_pango_font_metrics_unref(PangoFontMetrics* arg0)
{
  return pango_font_metrics_unref(arg0);
}

int Pure_pango_font_metrics_get_ascent(PangoFontMetrics* arg0)
{
  return pango_font_metrics_get_ascent(arg0);
}

int Pure_pango_font_metrics_get_descent(PangoFontMetrics* arg0)
{
  return pango_font_metrics_get_descent(arg0);
}

int Pure_pango_font_metrics_get_approximate_char_width(PangoFontMetrics* arg0)
{
  return pango_font_metrics_get_approximate_char_width(arg0);
}

int Pure_pango_font_metrics_get_approximate_digit_width(PangoFontMetrics* arg0)
{
  return pango_font_metrics_get_approximate_digit_width(arg0);
}

int Pure_pango_font_metrics_get_underline_position(PangoFontMetrics* arg0)
{
  return pango_font_metrics_get_underline_position(arg0);
}

int Pure_pango_font_metrics_get_underline_thickness(PangoFontMetrics* arg0)
{
  return pango_font_metrics_get_underline_thickness(arg0);
}

int Pure_pango_font_metrics_get_strikethrough_position(PangoFontMetrics* arg0)
{
  return pango_font_metrics_get_strikethrough_position(arg0);
}

int Pure_pango_font_metrics_get_strikethrough_thickness(PangoFontMetrics* arg0)
{
  return pango_font_metrics_get_strikethrough_thickness(arg0);
}

unsigned long Pure_pango_font_family_get_type()
{
  return pango_font_family_get_type();
}

void Pure_pango_font_family_list_faces(PangoFontFamily* arg0, PangoFontFace*** arg1, int* arg2)
{
  return pango_font_family_list_faces(arg0, arg1, arg2);
}

char const* Pure_pango_font_family_get_name(PangoFontFamily* arg0)
{
  return pango_font_family_get_name(arg0);
}

int Pure_pango_font_family_is_monospace(PangoFontFamily* arg0)
{
  return pango_font_family_is_monospace(arg0);
}

unsigned long Pure_pango_font_face_get_type()
{
  return pango_font_face_get_type();
}

PangoFontDescription* Pure_pango_font_face_describe(PangoFontFace* arg0)
{
  return pango_font_face_describe(arg0);
}

char const* Pure_pango_font_face_get_face_name(PangoFontFace* arg0)
{
  return pango_font_face_get_face_name(arg0);
}

void Pure_pango_font_face_list_sizes(PangoFontFace* arg0, int** arg1, int* arg2)
{
  return pango_font_face_list_sizes(arg0, arg1, arg2);
}

int Pure_pango_font_face_is_synthesized(PangoFontFace* arg0)
{
  return pango_font_face_is_synthesized(arg0);
}

unsigned long Pure_pango_font_get_type()
{
  return pango_font_get_type();
}

PangoFontDescription* Pure_pango_font_describe(PangoFont* arg0)
{
  return pango_font_describe(arg0);
}

PangoFontDescription* Pure_pango_font_describe_with_absolute_size(PangoFont* arg0)
{
  return pango_font_describe_with_absolute_size(arg0);
}

PangoCoverage* Pure_pango_font_get_coverage(PangoFont* arg0, PangoLanguage* arg1)
{
  return pango_font_get_coverage(arg0, arg1);
}

PangoEngineShape* Pure_pango_font_find_shaper(PangoFont* arg0, PangoLanguage* arg1, unsigned int arg2)
{
  return pango_font_find_shaper(arg0, arg1, arg2);
}

PangoFontMetrics* Pure_pango_font_get_metrics(PangoFont* arg0, PangoLanguage* arg1)
{
  return pango_font_get_metrics(arg0, arg1);
}

void Pure_pango_font_get_glyph_extents(PangoFont* arg0, unsigned int arg1, PangoRectangle* arg2, PangoRectangle* arg3)
{
  return pango_font_get_glyph_extents(arg0, arg1, arg2, arg3);
}

PangoFontMap* Pure_pango_font_get_font_map(PangoFont* arg0)
{
  return pango_font_get_font_map(arg0);
}

unsigned long Pure_pango_color_get_type()
{
  return pango_color_get_type();
}

PangoColor* Pure_pango_color_copy(PangoColor const* arg0)
{
  return pango_color_copy(arg0);
}

void Pure_pango_color_free(PangoColor* arg0)
{
  return pango_color_free(arg0);
}

int Pure_pango_color_parse(PangoColor* arg0, char const* arg1)
{
  return pango_color_parse(arg0, arg1);
}

char* Pure_pango_color_to_string(PangoColor const* arg0)
{
  return pango_color_to_string(arg0);
}

unsigned int Pure_pango_attr_type_register(char const* arg0)
{
  return pango_attr_type_register(arg0);
}

char const* Pure_pango_attr_type_get_name(unsigned int arg0)
{
  return pango_attr_type_get_name(arg0);
}

void Pure_pango_attribute_init(PangoAttribute* arg0, PangoAttrClass const* arg1)
{
  return pango_attribute_init(arg0, arg1);
}

PangoAttribute* Pure_pango_attribute_copy(PangoAttribute const* arg0)
{
  return pango_attribute_copy(arg0);
}

void Pure_pango_attribute_destroy(PangoAttribute* arg0)
{
  return pango_attribute_destroy(arg0);
}

int Pure_pango_attribute_equal(PangoAttribute const* arg0, PangoAttribute const* arg1)
{
  return pango_attribute_equal(arg0, arg1);
}

PangoAttribute* Pure_pango_attr_language_new(PangoLanguage* arg0)
{
  return pango_attr_language_new(arg0);
}

PangoAttribute* Pure_pango_attr_family_new(char const* arg0)
{
  return pango_attr_family_new(arg0);
}

PangoAttribute* Pure_pango_attr_foreground_new(unsigned short arg0, unsigned short arg1, unsigned short arg2)
{
  return pango_attr_foreground_new(arg0, arg1, arg2);
}

PangoAttribute* Pure_pango_attr_background_new(unsigned short arg0, unsigned short arg1, unsigned short arg2)
{
  return pango_attr_background_new(arg0, arg1, arg2);
}

PangoAttribute* Pure_pango_attr_size_new(int arg0)
{
  return pango_attr_size_new(arg0);
}

PangoAttribute* Pure_pango_attr_size_new_absolute(int arg0)
{
  return pango_attr_size_new_absolute(arg0);
}

PangoAttribute* Pure_pango_attr_style_new(unsigned int arg0)
{
  return pango_attr_style_new(arg0);
}

PangoAttribute* Pure_pango_attr_weight_new(unsigned int arg0)
{
  return pango_attr_weight_new(arg0);
}

PangoAttribute* Pure_pango_attr_variant_new(unsigned int arg0)
{
  return pango_attr_variant_new(arg0);
}

PangoAttribute* Pure_pango_attr_stretch_new(unsigned int arg0)
{
  return pango_attr_stretch_new(arg0);
}

PangoAttribute* Pure_pango_attr_font_desc_new(PangoFontDescription const* arg0)
{
  return pango_attr_font_desc_new(arg0);
}

PangoAttribute* Pure_pango_attr_underline_new(unsigned int arg0)
{
  return pango_attr_underline_new(arg0);
}

PangoAttribute* Pure_pango_attr_underline_color_new(unsigned short arg0, unsigned short arg1, unsigned short arg2)
{
  return pango_attr_underline_color_new(arg0, arg1, arg2);
}

PangoAttribute* Pure_pango_attr_strikethrough_new(int arg0)
{
  return pango_attr_strikethrough_new(arg0);
}

PangoAttribute* Pure_pango_attr_strikethrough_color_new(unsigned short arg0, unsigned short arg1, unsigned short arg2)
{
  return pango_attr_strikethrough_color_new(arg0, arg1, arg2);
}

PangoAttribute* Pure_pango_attr_rise_new(int arg0)
{
  return pango_attr_rise_new(arg0);
}

PangoAttribute* Pure_pango_attr_scale_new(double arg0)
{
  return pango_attr_scale_new(arg0);
}

PangoAttribute* Pure_pango_attr_fallback_new(int arg0)
{
  return pango_attr_fallback_new(arg0);
}

PangoAttribute* Pure_pango_attr_letter_spacing_new(int arg0)
{
  return pango_attr_letter_spacing_new(arg0);
}

PangoAttribute* Pure_pango_attr_shape_new(PangoRectangle const* arg0, PangoRectangle const* arg1)
{
  return pango_attr_shape_new(arg0, arg1);
}

PangoAttribute* Pure_pango_attr_shape_new_with_data(PangoRectangle const* arg0, PangoRectangle const* arg1, void* arg2, void* arg3, void* arg4)
{
  return pango_attr_shape_new_with_data(arg0, arg1, arg2, arg3, arg4);
}

PangoAttribute* Pure_pango_attr_gravity_new(unsigned int arg0)
{
  return pango_attr_gravity_new(arg0);
}

PangoAttribute* Pure_pango_attr_gravity_hint_new(unsigned int arg0)
{
  return pango_attr_gravity_hint_new(arg0);
}

unsigned long Pure_pango_attr_list_get_type()
{
  return pango_attr_list_get_type();
}

PangoAttrList* Pure_pango_attr_list_new()
{
  return pango_attr_list_new();
}

PangoAttrList* Pure_pango_attr_list_ref(PangoAttrList* arg0)
{
  return pango_attr_list_ref(arg0);
}

void Pure_pango_attr_list_unref(PangoAttrList* arg0)
{
  return pango_attr_list_unref(arg0);
}

PangoAttrList* Pure_pango_attr_list_copy(PangoAttrList* arg0)
{
  return pango_attr_list_copy(arg0);
}

void Pure_pango_attr_list_insert(PangoAttrList* arg0, PangoAttribute* arg1)
{
  return pango_attr_list_insert(arg0, arg1);
}

void Pure_pango_attr_list_insert_before(PangoAttrList* arg0, PangoAttribute* arg1)
{
  return pango_attr_list_insert_before(arg0, arg1);
}

void Pure_pango_attr_list_change(PangoAttrList* arg0, PangoAttribute* arg1)
{
  return pango_attr_list_change(arg0, arg1);
}

void Pure_pango_attr_list_splice(PangoAttrList* arg0, PangoAttrList* arg1, int arg2, int arg3)
{
  return pango_attr_list_splice(arg0, arg1, arg2, arg3);
}

PangoAttrList* Pure_pango_attr_list_filter(PangoAttrList* arg0, void* arg1, void* arg2)
{
  return pango_attr_list_filter(arg0, arg1, arg2);
}

PangoAttrIterator* Pure_pango_attr_list_get_iterator(PangoAttrList* arg0)
{
  return pango_attr_list_get_iterator(arg0);
}

void Pure_pango_attr_iterator_range(PangoAttrIterator* arg0, int* arg1, int* arg2)
{
  return pango_attr_iterator_range(arg0, arg1, arg2);
}

int Pure_pango_attr_iterator_next(PangoAttrIterator* arg0)
{
  return pango_attr_iterator_next(arg0);
}

PangoAttrIterator* Pure_pango_attr_iterator_copy(PangoAttrIterator* arg0)
{
  return pango_attr_iterator_copy(arg0);
}

void Pure_pango_attr_iterator_destroy(PangoAttrIterator* arg0)
{
  return pango_attr_iterator_destroy(arg0);
}

PangoAttribute* Pure_pango_attr_iterator_get(PangoAttrIterator* arg0, unsigned int arg1)
{
  return pango_attr_iterator_get(arg0, arg1);
}

void Pure_pango_attr_iterator_get_font(PangoAttrIterator* arg0, PangoFontDescription* arg1, PangoLanguage** arg2, GSList** arg3)
{
  return pango_attr_iterator_get_font(arg0, arg1, arg2, arg3);
}

GSList* Pure_pango_attr_iterator_get_attrs(PangoAttrIterator* arg0)
{
  return pango_attr_iterator_get_attrs(arg0);
}

int Pure_pango_parse_markup(char const* arg0, int arg1, unsigned int arg2, PangoAttrList** arg3, char** arg4, unsigned int* arg5, GError** arg6)
{
  return pango_parse_markup(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

unsigned long Pure_pango_item_get_type()
{
  return pango_item_get_type();
}

PangoItem* Pure_pango_item_new()
{
  return pango_item_new();
}

PangoItem* Pure_pango_item_copy(PangoItem* arg0)
{
  return pango_item_copy(arg0);
}

void Pure_pango_item_free(PangoItem* arg0)
{
  return pango_item_free(arg0);
}

PangoItem* Pure_pango_item_split(PangoItem* arg0, int arg1, int arg2)
{
  return pango_item_split(arg0, arg1, arg2);
}

void Pure_pango_break(char const* arg0, int arg1, PangoAnalysis* arg2, PangoLogAttr* arg3, int arg4)
{
  return pango_break(arg0, arg1, arg2, arg3, arg4);
}

void Pure_pango_find_paragraph_boundary(char const* arg0, int arg1, int* arg2, int* arg3)
{
  return pango_find_paragraph_boundary(arg0, arg1, arg2, arg3);
}

void Pure_pango_get_log_attrs(char const* arg0, int arg1, int arg2, PangoLanguage* arg3, PangoLogAttr* arg4, int arg5)
{
  return pango_get_log_attrs(arg0, arg1, arg2, arg3, arg4, arg5);
}

unsigned long Pure_pango_fontset_get_type()
{
  return pango_fontset_get_type();
}

PangoFont* Pure_pango_fontset_get_font(PangoFontset* arg0, unsigned int arg1)
{
  return pango_fontset_get_font(arg0, arg1);
}

PangoFontMetrics* Pure_pango_fontset_get_metrics(PangoFontset* arg0)
{
  return pango_fontset_get_metrics(arg0);
}

void Pure_pango_fontset_foreach(PangoFontset* arg0, void* arg1, void* arg2)
{
  return pango_fontset_foreach(arg0, arg1, arg2);
}

unsigned long Pure_pango_font_map_get_type()
{
  return pango_font_map_get_type();
}

PangoContext* Pure_pango_font_map_create_context(PangoFontMap* arg0)
{
  return pango_font_map_create_context(arg0);
}

PangoFont* Pure_pango_font_map_load_font(PangoFontMap* arg0, PangoContext* arg1, PangoFontDescription const* arg2)
{
  return pango_font_map_load_font(arg0, arg1, arg2);
}

PangoFontset* Pure_pango_font_map_load_fontset(PangoFontMap* arg0, PangoContext* arg1, PangoFontDescription const* arg2, PangoLanguage* arg3)
{
  return pango_font_map_load_fontset(arg0, arg1, arg2, arg3);
}

void Pure_pango_font_map_list_families(PangoFontMap* arg0, PangoFontFamily*** arg1, int* arg2)
{
  return pango_font_map_list_families(arg0, arg1, arg2);
}

unsigned long Pure_pango_context_get_type()
{
  return pango_context_get_type();
}

PangoContext* Pure_pango_context_new()
{
  return pango_context_new();
}

void Pure_pango_context_set_font_map(PangoContext* arg0, PangoFontMap* arg1)
{
  return pango_context_set_font_map(arg0, arg1);
}

PangoFontMap* Pure_pango_context_get_font_map(PangoContext* arg0)
{
  return pango_context_get_font_map(arg0);
}

void Pure_pango_context_list_families(PangoContext* arg0, PangoFontFamily*** arg1, int* arg2)
{
  return pango_context_list_families(arg0, arg1, arg2);
}

PangoFont* Pure_pango_context_load_font(PangoContext* arg0, PangoFontDescription const* arg1)
{
  return pango_context_load_font(arg0, arg1);
}

PangoFontset* Pure_pango_context_load_fontset(PangoContext* arg0, PangoFontDescription const* arg1, PangoLanguage* arg2)
{
  return pango_context_load_fontset(arg0, arg1, arg2);
}

PangoFontMetrics* Pure_pango_context_get_metrics(PangoContext* arg0, PangoFontDescription const* arg1, PangoLanguage* arg2)
{
  return pango_context_get_metrics(arg0, arg1, arg2);
}

void Pure_pango_context_set_font_description(PangoContext* arg0, PangoFontDescription const* arg1)
{
  return pango_context_set_font_description(arg0, arg1);
}

PangoFontDescription* Pure_pango_context_get_font_description(PangoContext* arg0)
{
  return pango_context_get_font_description(arg0);
}

PangoLanguage* Pure_pango_context_get_language(PangoContext* arg0)
{
  return pango_context_get_language(arg0);
}

void Pure_pango_context_set_language(PangoContext* arg0, PangoLanguage* arg1)
{
  return pango_context_set_language(arg0, arg1);
}

void Pure_pango_context_set_base_dir(PangoContext* arg0, unsigned int arg1)
{
  return pango_context_set_base_dir(arg0, arg1);
}

unsigned int Pure_pango_context_get_base_dir(PangoContext* arg0)
{
  return pango_context_get_base_dir(arg0);
}

void Pure_pango_context_set_base_gravity(PangoContext* arg0, unsigned int arg1)
{
  return pango_context_set_base_gravity(arg0, arg1);
}

unsigned int Pure_pango_context_get_base_gravity(PangoContext* arg0)
{
  return pango_context_get_base_gravity(arg0);
}

unsigned int Pure_pango_context_get_gravity(PangoContext* arg0)
{
  return pango_context_get_gravity(arg0);
}

void Pure_pango_context_set_gravity_hint(PangoContext* arg0, unsigned int arg1)
{
  return pango_context_set_gravity_hint(arg0, arg1);
}

unsigned int Pure_pango_context_get_gravity_hint(PangoContext* arg0)
{
  return pango_context_get_gravity_hint(arg0);
}

void Pure_pango_context_set_matrix(PangoContext* arg0, PangoMatrix const* arg1)
{
  return pango_context_set_matrix(arg0, arg1);
}

PangoMatrix const* Pure_pango_context_get_matrix(PangoContext* arg0)
{
  return pango_context_get_matrix(arg0);
}

GList* Pure_pango_itemize(PangoContext* arg0, char const* arg1, int arg2, int arg3, PangoAttrList* arg4, PangoAttrIterator* arg5)
{
  return pango_itemize(arg0, arg1, arg2, arg3, arg4, arg5);
}

GList* Pure_pango_itemize_with_base_dir(PangoContext* arg0, unsigned int arg1, char const* arg2, int arg3, int arg4, PangoAttrList* arg5, PangoAttrIterator* arg6)
{
  return pango_itemize_with_base_dir(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

PangoGlyphString* Pure_pango_glyph_string_new()
{
  return pango_glyph_string_new();
}

void Pure_pango_glyph_string_set_size(PangoGlyphString* arg0, int arg1)
{
  return pango_glyph_string_set_size(arg0, arg1);
}

unsigned long Pure_pango_glyph_string_get_type()
{
  return pango_glyph_string_get_type();
}

PangoGlyphString* Pure_pango_glyph_string_copy(PangoGlyphString* arg0)
{
  return pango_glyph_string_copy(arg0);
}

void Pure_pango_glyph_string_free(PangoGlyphString* arg0)
{
  return pango_glyph_string_free(arg0);
}

void Pure_pango_glyph_string_extents(PangoGlyphString* arg0, PangoFont* arg1, PangoRectangle* arg2, PangoRectangle* arg3)
{
  return pango_glyph_string_extents(arg0, arg1, arg2, arg3);
}

int Pure_pango_glyph_string_get_width(PangoGlyphString* arg0)
{
  return pango_glyph_string_get_width(arg0);
}

void Pure_pango_glyph_string_extents_range(PangoGlyphString* arg0, int arg1, int arg2, PangoFont* arg3, PangoRectangle* arg4, PangoRectangle* arg5)
{
  return pango_glyph_string_extents_range(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_pango_glyph_string_get_logical_widths(PangoGlyphString* arg0, char const* arg1, int arg2, int arg3, int* arg4)
{
  return pango_glyph_string_get_logical_widths(arg0, arg1, arg2, arg3, arg4);
}

void Pure_pango_glyph_string_index_to_x(PangoGlyphString* arg0, char* arg1, int arg2, PangoAnalysis* arg3, int arg4, int arg5, int* arg6)
{
  return pango_glyph_string_index_to_x(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_pango_glyph_string_x_to_index(PangoGlyphString* arg0, char* arg1, int arg2, PangoAnalysis* arg3, int arg4, int* arg5, int* arg6)
{
  return pango_glyph_string_x_to_index(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_pango_shape(char const* arg0, int arg1, PangoAnalysis const* arg2, PangoGlyphString* arg3)
{
  return pango_shape(arg0, arg1, arg2, arg3);
}

GList* Pure_pango_reorder_items(GList* arg0)
{
  return pango_reorder_items(arg0);
}

unsigned long Pure_pango_attr_type_get_type()
{
  return pango_attr_type_get_type();
}

unsigned long Pure_pango_underline_get_type()
{
  return pango_underline_get_type();
}

unsigned long Pure_pango_bidi_type_get_type()
{
  return pango_bidi_type_get_type();
}

unsigned long Pure_pango_direction_get_type()
{
  return pango_direction_get_type();
}

unsigned long Pure_pango_coverage_level_get_type()
{
  return pango_coverage_level_get_type();
}

unsigned long Pure_pango_style_get_type()
{
  return pango_style_get_type();
}

unsigned long Pure_pango_variant_get_type()
{
  return pango_variant_get_type();
}

unsigned long Pure_pango_weight_get_type()
{
  return pango_weight_get_type();
}

unsigned long Pure_pango_stretch_get_type()
{
  return pango_stretch_get_type();
}

unsigned long Pure_pango_font_mask_get_type()
{
  return pango_font_mask_get_type();
}

unsigned long Pure_pango_gravity_get_type()
{
  return pango_gravity_get_type();
}

unsigned long Pure_pango_gravity_hint_get_type()
{
  return pango_gravity_hint_get_type();
}

unsigned long Pure_pango_alignment_get_type()
{
  return pango_alignment_get_type();
}

unsigned long Pure_pango_wrap_mode_get_type()
{
  return pango_wrap_mode_get_type();
}

unsigned long Pure_pango_ellipsize_mode_get_type()
{
  return pango_ellipsize_mode_get_type();
}

unsigned long Pure_pango_render_part_get_type()
{
  return pango_render_part_get_type();
}

unsigned long Pure_pango_script_get_type()
{
  return pango_script_get_type();
}

unsigned long Pure_pango_tab_align_get_type()
{
  return pango_tab_align_get_type();
}

unsigned long Pure_pango_glyph_item_get_type()
{
  return pango_glyph_item_get_type();
}

PangoGlyphItem* Pure_pango_glyph_item_split(PangoGlyphItem* arg0, char const* arg1, int arg2)
{
  return pango_glyph_item_split(arg0, arg1, arg2);
}

PangoGlyphItem* Pure_pango_glyph_item_copy(PangoGlyphItem* arg0)
{
  return pango_glyph_item_copy(arg0);
}

void Pure_pango_glyph_item_free(PangoGlyphItem* arg0)
{
  return pango_glyph_item_free(arg0);
}

GSList* Pure_pango_glyph_item_apply_attrs(PangoGlyphItem* arg0, char const* arg1, PangoAttrList* arg2)
{
  return pango_glyph_item_apply_attrs(arg0, arg1, arg2);
}

void Pure_pango_glyph_item_letter_space(PangoGlyphItem* arg0, char const* arg1, PangoLogAttr* arg2, int arg3)
{
  return pango_glyph_item_letter_space(arg0, arg1, arg2, arg3);
}

unsigned long Pure_pango_glyph_item_iter_get_type()
{
  return pango_glyph_item_iter_get_type();
}

PangoGlyphItemIter* Pure_pango_glyph_item_iter_copy(PangoGlyphItemIter* arg0)
{
  return pango_glyph_item_iter_copy(arg0);
}

void Pure_pango_glyph_item_iter_free(PangoGlyphItemIter* arg0)
{
  return pango_glyph_item_iter_free(arg0);
}

int Pure_pango_glyph_item_iter_init_start(PangoGlyphItemIter* arg0, PangoGlyphItem* arg1, char const* arg2)
{
  return pango_glyph_item_iter_init_start(arg0, arg1, arg2);
}

int Pure_pango_glyph_item_iter_init_end(PangoGlyphItemIter* arg0, PangoGlyphItem* arg1, char const* arg2)
{
  return pango_glyph_item_iter_init_end(arg0, arg1, arg2);
}

int Pure_pango_glyph_item_iter_next_cluster(PangoGlyphItemIter* arg0)
{
  return pango_glyph_item_iter_next_cluster(arg0);
}

int Pure_pango_glyph_item_iter_prev_cluster(PangoGlyphItemIter* arg0)
{
  return pango_glyph_item_iter_prev_cluster(arg0);
}

PangoTabArray* Pure_pango_tab_array_new(int arg0, int arg1)
{
  return pango_tab_array_new(arg0, arg1);
}

PangoTabArray* Pure_pango_tab_array_new_with_positions(int arg0, int arg1, unsigned int arg2, int arg3)
{
  return pango_tab_array_new_with_positions(arg0, arg1, arg2, arg3);
}

unsigned long Pure_pango_tab_array_get_type()
{
  return pango_tab_array_get_type();
}

PangoTabArray* Pure_pango_tab_array_copy(PangoTabArray* arg0)
{
  return pango_tab_array_copy(arg0);
}

void Pure_pango_tab_array_free(PangoTabArray* arg0)
{
  return pango_tab_array_free(arg0);
}

int Pure_pango_tab_array_get_size(PangoTabArray* arg0)
{
  return pango_tab_array_get_size(arg0);
}

void Pure_pango_tab_array_resize(PangoTabArray* arg0, int arg1)
{
  return pango_tab_array_resize(arg0, arg1);
}

void Pure_pango_tab_array_set_tab(PangoTabArray* arg0, int arg1, unsigned int arg2, int arg3)
{
  return pango_tab_array_set_tab(arg0, arg1, arg2, arg3);
}

void Pure_pango_tab_array_get_tab(PangoTabArray* arg0, int arg1, unsigned int* arg2, int* arg3)
{
  return pango_tab_array_get_tab(arg0, arg1, arg2, arg3);
}

void Pure_pango_tab_array_get_tabs(PangoTabArray* arg0, unsigned int** arg1, int** arg2)
{
  return pango_tab_array_get_tabs(arg0, arg1, arg2);
}

int Pure_pango_tab_array_get_positions_in_pixels(PangoTabArray* arg0)
{
  return pango_tab_array_get_positions_in_pixels(arg0);
}

unsigned long Pure_pango_layout_get_type()
{
  return pango_layout_get_type();
}

PangoLayout* Pure_pango_layout_new(PangoContext* arg0)
{
  return pango_layout_new(arg0);
}

PangoLayout* Pure_pango_layout_copy(PangoLayout* arg0)
{
  return pango_layout_copy(arg0);
}

PangoContext* Pure_pango_layout_get_context(PangoLayout* arg0)
{
  return pango_layout_get_context(arg0);
}

void Pure_pango_layout_set_attributes(PangoLayout* arg0, PangoAttrList* arg1)
{
  return pango_layout_set_attributes(arg0, arg1);
}

PangoAttrList* Pure_pango_layout_get_attributes(PangoLayout* arg0)
{
  return pango_layout_get_attributes(arg0);
}

void Pure_pango_layout_set_text(PangoLayout* arg0, char const* arg1, int arg2)
{
  return pango_layout_set_text(arg0, arg1, arg2);
}

char const* Pure_pango_layout_get_text(PangoLayout* arg0)
{
  return pango_layout_get_text(arg0);
}

void Pure_pango_layout_set_markup(PangoLayout* arg0, char const* arg1, int arg2)
{
  return pango_layout_set_markup(arg0, arg1, arg2);
}

void Pure_pango_layout_set_markup_with_accel(PangoLayout* arg0, char const* arg1, int arg2, unsigned int arg3, unsigned int* arg4)
{
  return pango_layout_set_markup_with_accel(arg0, arg1, arg2, arg3, arg4);
}

void Pure_pango_layout_set_font_description(PangoLayout* arg0, PangoFontDescription const* arg1)
{
  return pango_layout_set_font_description(arg0, arg1);
}

PangoFontDescription const* Pure_pango_layout_get_font_description(PangoLayout* arg0)
{
  return pango_layout_get_font_description(arg0);
}

void Pure_pango_layout_set_width(PangoLayout* arg0, int arg1)
{
  return pango_layout_set_width(arg0, arg1);
}

int Pure_pango_layout_get_width(PangoLayout* arg0)
{
  return pango_layout_get_width(arg0);
}

void Pure_pango_layout_set_height(PangoLayout* arg0, int arg1)
{
  return pango_layout_set_height(arg0, arg1);
}

int Pure_pango_layout_get_height(PangoLayout* arg0)
{
  return pango_layout_get_height(arg0);
}

void Pure_pango_layout_set_wrap(PangoLayout* arg0, unsigned int arg1)
{
  return pango_layout_set_wrap(arg0, arg1);
}

unsigned int Pure_pango_layout_get_wrap(PangoLayout* arg0)
{
  return pango_layout_get_wrap(arg0);
}

int Pure_pango_layout_is_wrapped(PangoLayout* arg0)
{
  return pango_layout_is_wrapped(arg0);
}

void Pure_pango_layout_set_indent(PangoLayout* arg0, int arg1)
{
  return pango_layout_set_indent(arg0, arg1);
}

int Pure_pango_layout_get_indent(PangoLayout* arg0)
{
  return pango_layout_get_indent(arg0);
}

void Pure_pango_layout_set_spacing(PangoLayout* arg0, int arg1)
{
  return pango_layout_set_spacing(arg0, arg1);
}

int Pure_pango_layout_get_spacing(PangoLayout* arg0)
{
  return pango_layout_get_spacing(arg0);
}

void Pure_pango_layout_set_justify(PangoLayout* arg0, int arg1)
{
  return pango_layout_set_justify(arg0, arg1);
}

int Pure_pango_layout_get_justify(PangoLayout* arg0)
{
  return pango_layout_get_justify(arg0);
}

void Pure_pango_layout_set_auto_dir(PangoLayout* arg0, int arg1)
{
  return pango_layout_set_auto_dir(arg0, arg1);
}

int Pure_pango_layout_get_auto_dir(PangoLayout* arg0)
{
  return pango_layout_get_auto_dir(arg0);
}

void Pure_pango_layout_set_alignment(PangoLayout* arg0, unsigned int arg1)
{
  return pango_layout_set_alignment(arg0, arg1);
}

unsigned int Pure_pango_layout_get_alignment(PangoLayout* arg0)
{
  return pango_layout_get_alignment(arg0);
}

void Pure_pango_layout_set_tabs(PangoLayout* arg0, PangoTabArray* arg1)
{
  return pango_layout_set_tabs(arg0, arg1);
}

PangoTabArray* Pure_pango_layout_get_tabs(PangoLayout* arg0)
{
  return pango_layout_get_tabs(arg0);
}

void Pure_pango_layout_set_single_paragraph_mode(PangoLayout* arg0, int arg1)
{
  return pango_layout_set_single_paragraph_mode(arg0, arg1);
}

int Pure_pango_layout_get_single_paragraph_mode(PangoLayout* arg0)
{
  return pango_layout_get_single_paragraph_mode(arg0);
}

void Pure_pango_layout_set_ellipsize(PangoLayout* arg0, unsigned int arg1)
{
  return pango_layout_set_ellipsize(arg0, arg1);
}

unsigned int Pure_pango_layout_get_ellipsize(PangoLayout* arg0)
{
  return pango_layout_get_ellipsize(arg0);
}

int Pure_pango_layout_is_ellipsized(PangoLayout* arg0)
{
  return pango_layout_is_ellipsized(arg0);
}

int Pure_pango_layout_get_unknown_glyphs_count(PangoLayout* arg0)
{
  return pango_layout_get_unknown_glyphs_count(arg0);
}

void Pure_pango_layout_context_changed(PangoLayout* arg0)
{
  return pango_layout_context_changed(arg0);
}

void Pure_pango_layout_get_log_attrs(PangoLayout* arg0, PangoLogAttr** arg1, int* arg2)
{
  return pango_layout_get_log_attrs(arg0, arg1, arg2);
}

void Pure_pango_layout_index_to_pos(PangoLayout* arg0, int arg1, PangoRectangle* arg2)
{
  return pango_layout_index_to_pos(arg0, arg1, arg2);
}

void Pure_pango_layout_index_to_line_x(PangoLayout* arg0, int arg1, int arg2, int* arg3, int* arg4)
{
  return pango_layout_index_to_line_x(arg0, arg1, arg2, arg3, arg4);
}

void Pure_pango_layout_get_cursor_pos(PangoLayout* arg0, int arg1, PangoRectangle* arg2, PangoRectangle* arg3)
{
  return pango_layout_get_cursor_pos(arg0, arg1, arg2, arg3);
}

void Pure_pango_layout_move_cursor_visually(PangoLayout* arg0, int arg1, int arg2, int arg3, int arg4, int* arg5, int* arg6)
{
  return pango_layout_move_cursor_visually(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

int Pure_pango_layout_xy_to_index(PangoLayout* arg0, int arg1, int arg2, int* arg3, int* arg4)
{
  return pango_layout_xy_to_index(arg0, arg1, arg2, arg3, arg4);
}

void Pure_pango_layout_get_extents(PangoLayout* arg0, PangoRectangle* arg1, PangoRectangle* arg2)
{
  return pango_layout_get_extents(arg0, arg1, arg2);
}

void Pure_pango_layout_get_pixel_extents(PangoLayout* arg0, PangoRectangle* arg1, PangoRectangle* arg2)
{
  return pango_layout_get_pixel_extents(arg0, arg1, arg2);
}

void Pure_pango_layout_get_size(PangoLayout* arg0, int* arg1, int* arg2)
{
  return pango_layout_get_size(arg0, arg1, arg2);
}

void Pure_pango_layout_get_pixel_size(PangoLayout* arg0, int* arg1, int* arg2)
{
  return pango_layout_get_pixel_size(arg0, arg1, arg2);
}

int Pure_pango_layout_get_baseline(PangoLayout* arg0)
{
  return pango_layout_get_baseline(arg0);
}

int Pure_pango_layout_get_line_count(PangoLayout* arg0)
{
  return pango_layout_get_line_count(arg0);
}

PangoLayoutLine* Pure_pango_layout_get_line(PangoLayout* arg0, int arg1)
{
  return pango_layout_get_line(arg0, arg1);
}

PangoLayoutLine* Pure_pango_layout_get_line_readonly(PangoLayout* arg0, int arg1)
{
  return pango_layout_get_line_readonly(arg0, arg1);
}

GSList* Pure_pango_layout_get_lines(PangoLayout* arg0)
{
  return pango_layout_get_lines(arg0);
}

GSList* Pure_pango_layout_get_lines_readonly(PangoLayout* arg0)
{
  return pango_layout_get_lines_readonly(arg0);
}

unsigned long Pure_pango_layout_line_get_type()
{
  return pango_layout_line_get_type();
}

PangoLayoutLine* Pure_pango_layout_line_ref(PangoLayoutLine* arg0)
{
  return pango_layout_line_ref(arg0);
}

void Pure_pango_layout_line_unref(PangoLayoutLine* arg0)
{
  return pango_layout_line_unref(arg0);
}

int Pure_pango_layout_line_x_to_index(PangoLayoutLine* arg0, int arg1, int* arg2, int* arg3)
{
  return pango_layout_line_x_to_index(arg0, arg1, arg2, arg3);
}

void Pure_pango_layout_line_index_to_x(PangoLayoutLine* arg0, int arg1, int arg2, int* arg3)
{
  return pango_layout_line_index_to_x(arg0, arg1, arg2, arg3);
}

void Pure_pango_layout_line_get_x_ranges(PangoLayoutLine* arg0, int arg1, int arg2, int** arg3, int* arg4)
{
  return pango_layout_line_get_x_ranges(arg0, arg1, arg2, arg3, arg4);
}

void Pure_pango_layout_line_get_extents(PangoLayoutLine* arg0, PangoRectangle* arg1, PangoRectangle* arg2)
{
  return pango_layout_line_get_extents(arg0, arg1, arg2);
}

void Pure_pango_layout_line_get_pixel_extents(PangoLayoutLine* arg0, PangoRectangle* arg1, PangoRectangle* arg2)
{
  return pango_layout_line_get_pixel_extents(arg0, arg1, arg2);
}

unsigned long Pure_pango_layout_iter_get_type()
{
  return pango_layout_iter_get_type();
}

PangoLayoutIter* Pure_pango_layout_get_iter(PangoLayout* arg0)
{
  return pango_layout_get_iter(arg0);
}

PangoLayoutIter* Pure_pango_layout_iter_copy(PangoLayoutIter* arg0)
{
  return pango_layout_iter_copy(arg0);
}

void Pure_pango_layout_iter_free(PangoLayoutIter* arg0)
{
  return pango_layout_iter_free(arg0);
}

int Pure_pango_layout_iter_get_index(PangoLayoutIter* arg0)
{
  return pango_layout_iter_get_index(arg0);
}

PangoLayoutRun* Pure_pango_layout_iter_get_run(PangoLayoutIter* arg0)
{
  return pango_layout_iter_get_run(arg0);
}

PangoLayoutRun* Pure_pango_layout_iter_get_run_readonly(PangoLayoutIter* arg0)
{
  return pango_layout_iter_get_run_readonly(arg0);
}

PangoLayoutLine* Pure_pango_layout_iter_get_line(PangoLayoutIter* arg0)
{
  return pango_layout_iter_get_line(arg0);
}

PangoLayoutLine* Pure_pango_layout_iter_get_line_readonly(PangoLayoutIter* arg0)
{
  return pango_layout_iter_get_line_readonly(arg0);
}

int Pure_pango_layout_iter_at_last_line(PangoLayoutIter* arg0)
{
  return pango_layout_iter_at_last_line(arg0);
}

PangoLayout* Pure_pango_layout_iter_get_layout(PangoLayoutIter* arg0)
{
  return pango_layout_iter_get_layout(arg0);
}

int Pure_pango_layout_iter_next_char(PangoLayoutIter* arg0)
{
  return pango_layout_iter_next_char(arg0);
}

int Pure_pango_layout_iter_next_cluster(PangoLayoutIter* arg0)
{
  return pango_layout_iter_next_cluster(arg0);
}

int Pure_pango_layout_iter_next_run(PangoLayoutIter* arg0)
{
  return pango_layout_iter_next_run(arg0);
}

int Pure_pango_layout_iter_next_line(PangoLayoutIter* arg0)
{
  return pango_layout_iter_next_line(arg0);
}

void Pure_pango_layout_iter_get_char_extents(PangoLayoutIter* arg0, PangoRectangle* arg1)
{
  return pango_layout_iter_get_char_extents(arg0, arg1);
}

void Pure_pango_layout_iter_get_cluster_extents(PangoLayoutIter* arg0, PangoRectangle* arg1, PangoRectangle* arg2)
{
  return pango_layout_iter_get_cluster_extents(arg0, arg1, arg2);
}

void Pure_pango_layout_iter_get_run_extents(PangoLayoutIter* arg0, PangoRectangle* arg1, PangoRectangle* arg2)
{
  return pango_layout_iter_get_run_extents(arg0, arg1, arg2);
}

void Pure_pango_layout_iter_get_line_extents(PangoLayoutIter* arg0, PangoRectangle* arg1, PangoRectangle* arg2)
{
  return pango_layout_iter_get_line_extents(arg0, arg1, arg2);
}

void Pure_pango_layout_iter_get_line_yrange(PangoLayoutIter* arg0, int* arg1, int* arg2)
{
  return pango_layout_iter_get_line_yrange(arg0, arg1, arg2);
}

void Pure_pango_layout_iter_get_layout_extents(PangoLayoutIter* arg0, PangoRectangle* arg1, PangoRectangle* arg2)
{
  return pango_layout_iter_get_layout_extents(arg0, arg1, arg2);
}

int Pure_pango_layout_iter_get_baseline(PangoLayoutIter* arg0)
{
  return pango_layout_iter_get_baseline(arg0);
}

unsigned long Pure_pango_renderer_get_type()
{
  return pango_renderer_get_type();
}

void Pure_pango_renderer_draw_layout(PangoRenderer* arg0, PangoLayout* arg1, int arg2, int arg3)
{
  return pango_renderer_draw_layout(arg0, arg1, arg2, arg3);
}

void Pure_pango_renderer_draw_layout_line(PangoRenderer* arg0, PangoLayoutLine* arg1, int arg2, int arg3)
{
  return pango_renderer_draw_layout_line(arg0, arg1, arg2, arg3);
}

void Pure_pango_renderer_draw_glyphs(PangoRenderer* arg0, PangoFont* arg1, PangoGlyphString* arg2, int arg3, int arg4)
{
  return pango_renderer_draw_glyphs(arg0, arg1, arg2, arg3, arg4);
}

void Pure_pango_renderer_draw_glyph_item(PangoRenderer* arg0, char const* arg1, PangoGlyphItem* arg2, int arg3, int arg4)
{
  return pango_renderer_draw_glyph_item(arg0, arg1, arg2, arg3, arg4);
}

void Pure_pango_renderer_draw_rectangle(PangoRenderer* arg0, unsigned int arg1, int arg2, int arg3, int arg4, int arg5)
{
  return pango_renderer_draw_rectangle(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_pango_renderer_draw_error_underline(PangoRenderer* arg0, int arg1, int arg2, int arg3, int arg4)
{
  return pango_renderer_draw_error_underline(arg0, arg1, arg2, arg3, arg4);
}

void Pure_pango_renderer_draw_trapezoid(PangoRenderer* arg0, unsigned int arg1, double arg2, double arg3, double arg4, double arg5, double arg6, double arg7)
{
  return pango_renderer_draw_trapezoid(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_pango_renderer_draw_glyph(PangoRenderer* arg0, PangoFont* arg1, unsigned int arg2, double arg3, double arg4)
{
  return pango_renderer_draw_glyph(arg0, arg1, arg2, arg3, arg4);
}

void Pure_pango_renderer_activate(PangoRenderer* arg0)
{
  return pango_renderer_activate(arg0);
}

void Pure_pango_renderer_deactivate(PangoRenderer* arg0)
{
  return pango_renderer_deactivate(arg0);
}

void Pure_pango_renderer_part_changed(PangoRenderer* arg0, unsigned int arg1)
{
  return pango_renderer_part_changed(arg0, arg1);
}

void Pure_pango_renderer_set_color(PangoRenderer* arg0, unsigned int arg1, PangoColor const* arg2)
{
  return pango_renderer_set_color(arg0, arg1, arg2);
}

PangoColor* Pure_pango_renderer_get_color(PangoRenderer* arg0, unsigned int arg1)
{
  return pango_renderer_get_color(arg0, arg1);
}

void Pure_pango_renderer_set_matrix(PangoRenderer* arg0, PangoMatrix const* arg1)
{
  return pango_renderer_set_matrix(arg0, arg1);
}

PangoMatrix const* Pure_pango_renderer_get_matrix(PangoRenderer* arg0)
{
  return pango_renderer_get_matrix(arg0);
}

PangoLayout* Pure_pango_renderer_get_layout(PangoRenderer* arg0)
{
  return pango_renderer_get_layout(arg0);
}

PangoLayoutLine* Pure_pango_renderer_get_layout_line(PangoRenderer* arg0)
{
  return pango_renderer_get_layout_line(arg0);
}

char** Pure_pango_split_file_list(char const* arg0)
{
  return pango_split_file_list(arg0);
}

char* Pure_pango_trim_string(char const* arg0)
{
  return pango_trim_string(arg0);
}

int Pure_pango_read_line(FILE* arg0, GString* arg1)
{
  return pango_read_line(arg0, arg1);
}

int Pure_pango_skip_space(char const** arg0)
{
  return pango_skip_space(arg0);
}

int Pure_pango_scan_word(char const** arg0, GString* arg1)
{
  return pango_scan_word(arg0, arg1);
}

int Pure_pango_scan_string(char const** arg0, GString* arg1)
{
  return pango_scan_string(arg0, arg1);
}

int Pure_pango_scan_int(char const** arg0, int* arg1)
{
  return pango_scan_int(arg0, arg1);
}

int Pure_pango_parse_enum(unsigned long arg0, char const* arg1, int* arg2, int arg3, char** arg4)
{
  return pango_parse_enum(arg0, arg1, arg2, arg3, arg4);
}

int Pure_pango_parse_style(char const* arg0, unsigned int* arg1, int arg2)
{
  return pango_parse_style(arg0, arg1, arg2);
}

int Pure_pango_parse_variant(char const* arg0, unsigned int* arg1, int arg2)
{
  return pango_parse_variant(arg0, arg1, arg2);
}

int Pure_pango_parse_weight(char const* arg0, unsigned int* arg1, int arg2)
{
  return pango_parse_weight(arg0, arg1, arg2);
}

int Pure_pango_parse_stretch(char const* arg0, unsigned int* arg1, int arg2)
{
  return pango_parse_stretch(arg0, arg1, arg2);
}

void Pure_pango_quantize_line_geometry(int* arg0, int* arg1)
{
  return pango_quantize_line_geometry(arg0, arg1);
}

unsigned char* Pure_pango_log2vis_get_embedding_levels(char const* arg0, int arg1, unsigned int* arg2)
{
  return pango_log2vis_get_embedding_levels(arg0, arg1, arg2);
}

int Pure_pango_is_zero_width(unsigned int arg0)
{
  return pango_is_zero_width(arg0);
}

int Pure_pango_version()
{
  return pango_version();
}

char const* Pure_pango_version_string()
{
  return pango_version_string();
}

char const* Pure_pango_version_check(int arg0, int arg1, int arg2)
{
  return pango_version_check(arg0, arg1, arg2);
}
