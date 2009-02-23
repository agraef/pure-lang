#include <gdk/gdk.h>

unsigned long Pure_gdk_colormap_get_type()
{
  return gdk_colormap_get_type();
}

GdkColormap* Pure_gdk_colormap_new(GdkVisual* arg0, int arg1)
{
  return gdk_colormap_new(arg0, arg1);
}

GdkColormap* Pure_gdk_colormap_ref(GdkColormap* arg0)
{
  return gdk_colormap_ref(arg0);
}

void Pure_gdk_colormap_unref(GdkColormap* arg0)
{
  return gdk_colormap_unref(arg0);
}

GdkColormap* Pure_gdk_colormap_get_system()
{
  return gdk_colormap_get_system();
}

GdkScreen* Pure_gdk_colormap_get_screen(GdkColormap* arg0)
{
  return gdk_colormap_get_screen(arg0);
}

int Pure_gdk_colormap_get_system_size()
{
  return gdk_colormap_get_system_size();
}

void Pure_gdk_colormap_change(GdkColormap* arg0, int arg1)
{
  return gdk_colormap_change(arg0, arg1);
}

int Pure_gdk_colormap_alloc_colors(GdkColormap* arg0, GdkColor* arg1, int arg2, int arg3, int arg4, int* arg5)
{
  return gdk_colormap_alloc_colors(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gdk_colormap_alloc_color(GdkColormap* arg0, GdkColor* arg1, int arg2, int arg3)
{
  return gdk_colormap_alloc_color(arg0, arg1, arg2, arg3);
}

void Pure_gdk_colormap_free_colors(GdkColormap* arg0, GdkColor const* arg1, int arg2)
{
  return gdk_colormap_free_colors(arg0, arg1, arg2);
}

void Pure_gdk_colormap_query_color(GdkColormap* arg0, unsigned long arg1, GdkColor* arg2)
{
  return gdk_colormap_query_color(arg0, arg1, arg2);
}

GdkVisual* Pure_gdk_colormap_get_visual(GdkColormap* arg0)
{
  return gdk_colormap_get_visual(arg0);
}

GdkColor* Pure_gdk_color_copy(GdkColor const* arg0)
{
  return gdk_color_copy(arg0);
}

void Pure_gdk_color_free(GdkColor* arg0)
{
  return gdk_color_free(arg0);
}

int Pure_gdk_color_parse(char const* arg0, GdkColor* arg1)
{
  return gdk_color_parse(arg0, arg1);
}

unsigned int Pure_gdk_color_hash(GdkColor const* arg0)
{
  return gdk_color_hash(arg0);
}

int Pure_gdk_color_equal(GdkColor const* arg0, GdkColor const* arg1)
{
  return gdk_color_equal(arg0, arg1);
}

char* Pure_gdk_color_to_string(GdkColor const* arg0)
{
  return gdk_color_to_string(arg0);
}

unsigned long Pure_gdk_color_get_type()
{
  return gdk_color_get_type();
}

void Pure_gdk_colors_store(GdkColormap* arg0, GdkColor* arg1, int arg2)
{
  return gdk_colors_store(arg0, arg1, arg2);
}

int Pure_gdk_color_white(GdkColormap* arg0, GdkColor* arg1)
{
  return gdk_color_white(arg0, arg1);
}

int Pure_gdk_color_black(GdkColormap* arg0, GdkColor* arg1)
{
  return gdk_color_black(arg0, arg1);
}

int Pure_gdk_color_alloc(GdkColormap* arg0, GdkColor* arg1)
{
  return gdk_color_alloc(arg0, arg1);
}

int Pure_gdk_color_change(GdkColormap* arg0, GdkColor* arg1)
{
  return gdk_color_change(arg0, arg1);
}

int Pure_gdk_colors_alloc(GdkColormap* arg0, int arg1, unsigned long* arg2, int arg3, unsigned long* arg4, int arg5)
{
  return gdk_colors_alloc(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gdk_colors_free(GdkColormap* arg0, unsigned long* arg1, int arg2, unsigned long arg3)
{
  return gdk_colors_free(arg0, arg1, arg2, arg3);
}

unsigned long Pure_gdk_drag_context_get_type()
{
  return gdk_drag_context_get_type();
}

GdkDragContext* Pure_gdk_drag_context_new()
{
  return gdk_drag_context_new();
}

void Pure_gdk_drag_context_ref(GdkDragContext* arg0)
{
  return gdk_drag_context_ref(arg0);
}

void Pure_gdk_drag_context_unref(GdkDragContext* arg0)
{
  return gdk_drag_context_unref(arg0);
}

void Pure_gdk_drag_status(GdkDragContext* arg0, unsigned int arg1, unsigned int arg2)
{
  return gdk_drag_status(arg0, arg1, arg2);
}

void Pure_gdk_drop_reply(GdkDragContext* arg0, int arg1, unsigned int arg2)
{
  return gdk_drop_reply(arg0, arg1, arg2);
}

void Pure_gdk_drop_finish(GdkDragContext* arg0, int arg1, unsigned int arg2)
{
  return gdk_drop_finish(arg0, arg1, arg2);
}

struct _GdkAtom* Pure_gdk_drag_get_selection(GdkDragContext* arg0)
{
  return gdk_drag_get_selection(arg0);
}

GdkDragContext* Pure_gdk_drag_begin(GdkWindow* arg0, GList* arg1)
{
  return gdk_drag_begin(arg0, arg1);
}

unsigned int Pure_gdk_drag_get_protocol_for_display(GdkDisplay* arg0, unsigned int arg1, unsigned int* arg2)
{
  return gdk_drag_get_protocol_for_display(arg0, arg1, arg2);
}

void Pure_gdk_drag_find_window_for_screen(GdkDragContext* arg0, GdkWindow* arg1, GdkScreen* arg2, int arg3, int arg4, GdkWindow** arg5, unsigned int* arg6)
{
  return gdk_drag_find_window_for_screen(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

unsigned int Pure_gdk_drag_get_protocol(unsigned int arg0, unsigned int* arg1)
{
  return gdk_drag_get_protocol(arg0, arg1);
}

void Pure_gdk_drag_find_window(GdkDragContext* arg0, GdkWindow* arg1, int arg2, int arg3, GdkWindow** arg4, unsigned int* arg5)
{
  return gdk_drag_find_window(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gdk_drag_motion(GdkDragContext* arg0, GdkWindow* arg1, unsigned int arg2, int arg3, int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7)
{
  return gdk_drag_motion(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gdk_drag_drop(GdkDragContext* arg0, unsigned int arg1)
{
  return gdk_drag_drop(arg0, arg1);
}

void Pure_gdk_drag_abort(GdkDragContext* arg0, unsigned int arg1)
{
  return gdk_drag_abort(arg0, arg1);
}

int Pure_gdk_drag_drop_succeeded(GdkDragContext* arg0)
{
  return gdk_drag_drop_succeeded(arg0);
}

unsigned long Pure_gdk_device_get_type()
{
  return gdk_device_get_type();
}

GList* Pure_gdk_devices_list()
{
  return gdk_devices_list();
}

void Pure_gdk_device_set_source(GdkDevice* arg0, unsigned int arg1)
{
  return gdk_device_set_source(arg0, arg1);
}

int Pure_gdk_device_set_mode(GdkDevice* arg0, unsigned int arg1)
{
  return gdk_device_set_mode(arg0, arg1);
}

void Pure_gdk_device_set_key(GdkDevice* arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  return gdk_device_set_key(arg0, arg1, arg2, arg3);
}

void Pure_gdk_device_set_axis_use(GdkDevice* arg0, unsigned int arg1, unsigned int arg2)
{
  return gdk_device_set_axis_use(arg0, arg1, arg2);
}

void Pure_gdk_device_get_state(GdkDevice* arg0, GdkWindow* arg1, double* arg2, unsigned int* arg3)
{
  return gdk_device_get_state(arg0, arg1, arg2, arg3);
}

int Pure_gdk_device_get_history(GdkDevice* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkTimeCoord*** arg4, int* arg5)
{
  return gdk_device_get_history(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gdk_device_free_history(GdkTimeCoord** arg0, int arg1)
{
  return gdk_device_free_history(arg0, arg1);
}

int Pure_gdk_device_get_axis(GdkDevice* arg0, double* arg1, unsigned int arg2, double* arg3)
{
  return gdk_device_get_axis(arg0, arg1, arg2, arg3);
}

void Pure_gdk_input_set_extension_events(GdkWindow* arg0, int arg1, unsigned int arg2)
{
  return gdk_input_set_extension_events(arg0, arg1, arg2);
}

GdkDevice* Pure_gdk_device_get_core_pointer()
{
  return gdk_device_get_core_pointer();
}

unsigned long Pure_gdk_event_get_type()
{
  return gdk_event_get_type();
}

int Pure_gdk_events_pending()
{
  return gdk_events_pending();
}

GdkEvent* Pure_gdk_event_get()
{
  return gdk_event_get();
}

GdkEvent* Pure_gdk_event_peek()
{
  return gdk_event_peek();
}

GdkEvent* Pure_gdk_event_get_graphics_expose(GdkWindow* arg0)
{
  return gdk_event_get_graphics_expose(arg0);
}

void Pure_gdk_event_put(GdkEvent const* arg0)
{
  return gdk_event_put(arg0);
}

GdkEvent* Pure_gdk_event_new(int arg0)
{
  return gdk_event_new(arg0);
}

GdkEvent* Pure_gdk_event_copy(GdkEvent const* arg0)
{
  return gdk_event_copy(arg0);
}

void Pure_gdk_event_free(GdkEvent* arg0)
{
  return gdk_event_free(arg0);
}

unsigned int Pure_gdk_event_get_time(GdkEvent const* arg0)
{
  return gdk_event_get_time(arg0);
}

int Pure_gdk_event_get_state(GdkEvent const* arg0, unsigned int* arg1)
{
  return gdk_event_get_state(arg0, arg1);
}

int Pure_gdk_event_get_coords(GdkEvent const* arg0, double* arg1, double* arg2)
{
  return gdk_event_get_coords(arg0, arg1, arg2);
}

int Pure_gdk_event_get_root_coords(GdkEvent const* arg0, double* arg1, double* arg2)
{
  return gdk_event_get_root_coords(arg0, arg1, arg2);
}

int Pure_gdk_event_get_axis(GdkEvent const* arg0, unsigned int arg1, double* arg2)
{
  return gdk_event_get_axis(arg0, arg1, arg2);
}

void Pure_gdk_event_request_motions(GdkEventMotion const* arg0)
{
  return gdk_event_request_motions(arg0);
}

void Pure_gdk_event_handler_set(void* arg0, void* arg1, void* arg2)
{
  return gdk_event_handler_set(arg0, arg1, arg2);
}

void Pure_gdk_event_set_screen(GdkEvent* arg0, GdkScreen* arg1)
{
  return gdk_event_set_screen(arg0, arg1);
}

GdkScreen* Pure_gdk_event_get_screen(GdkEvent const* arg0)
{
  return gdk_event_get_screen(arg0);
}

void Pure_gdk_set_show_events(int arg0)
{
  return gdk_set_show_events(arg0);
}

int Pure_gdk_get_show_events()
{
  return gdk_get_show_events();
}

void Pure_gdk_add_client_message_filter(struct _GdkAtom* arg0, void* arg1, void* arg2)
{
  return gdk_add_client_message_filter(arg0, arg1, arg2);
}

int Pure_gdk_setting_get(char const* arg0, GValue* arg1)
{
  return gdk_setting_get(arg0, arg1);
}

unsigned long Pure_gdk_display_get_type()
{
  return gdk_display_get_type();
}

GdkDisplay* Pure_gdk_display_open(char const* arg0)
{
  return gdk_display_open(arg0);
}

char const* Pure_gdk_display_get_name(GdkDisplay* arg0)
{
  return gdk_display_get_name(arg0);
}

int Pure_gdk_display_get_n_screens(GdkDisplay* arg0)
{
  return gdk_display_get_n_screens(arg0);
}

GdkScreen* Pure_gdk_display_get_screen(GdkDisplay* arg0, int arg1)
{
  return gdk_display_get_screen(arg0, arg1);
}

GdkScreen* Pure_gdk_display_get_default_screen(GdkDisplay* arg0)
{
  return gdk_display_get_default_screen(arg0);
}

void Pure_gdk_display_pointer_ungrab(GdkDisplay* arg0, unsigned int arg1)
{
  return gdk_display_pointer_ungrab(arg0, arg1);
}

void Pure_gdk_display_keyboard_ungrab(GdkDisplay* arg0, unsigned int arg1)
{
  return gdk_display_keyboard_ungrab(arg0, arg1);
}

int Pure_gdk_display_pointer_is_grabbed(GdkDisplay* arg0)
{
  return gdk_display_pointer_is_grabbed(arg0);
}

void Pure_gdk_display_beep(GdkDisplay* arg0)
{
  return gdk_display_beep(arg0);
}

void Pure_gdk_display_sync(GdkDisplay* arg0)
{
  return gdk_display_sync(arg0);
}

void Pure_gdk_display_flush(GdkDisplay* arg0)
{
  return gdk_display_flush(arg0);
}

void Pure_gdk_display_close(GdkDisplay* arg0)
{
  return gdk_display_close(arg0);
}

GList* Pure_gdk_display_list_devices(GdkDisplay* arg0)
{
  return gdk_display_list_devices(arg0);
}

GdkEvent* Pure_gdk_display_get_event(GdkDisplay* arg0)
{
  return gdk_display_get_event(arg0);
}

GdkEvent* Pure_gdk_display_peek_event(GdkDisplay* arg0)
{
  return gdk_display_peek_event(arg0);
}

void Pure_gdk_display_put_event(GdkDisplay* arg0, GdkEvent const* arg1)
{
  return gdk_display_put_event(arg0, arg1);
}

void Pure_gdk_display_add_client_message_filter(GdkDisplay* arg0, struct _GdkAtom* arg1, void* arg2, void* arg3)
{
  return gdk_display_add_client_message_filter(arg0, arg1, arg2, arg3);
}

void Pure_gdk_display_set_double_click_time(GdkDisplay* arg0, unsigned int arg1)
{
  return gdk_display_set_double_click_time(arg0, arg1);
}

void Pure_gdk_display_set_double_click_distance(GdkDisplay* arg0, unsigned int arg1)
{
  return gdk_display_set_double_click_distance(arg0, arg1);
}

GdkDisplay* Pure_gdk_display_get_default()
{
  return gdk_display_get_default();
}

GdkDevice* Pure_gdk_display_get_core_pointer(GdkDisplay* arg0)
{
  return gdk_display_get_core_pointer(arg0);
}

void Pure_gdk_display_get_pointer(GdkDisplay* arg0, GdkScreen** arg1, int* arg2, int* arg3, unsigned int* arg4)
{
  return gdk_display_get_pointer(arg0, arg1, arg2, arg3, arg4);
}

GdkWindow* Pure_gdk_display_get_window_at_pointer(GdkDisplay* arg0, int* arg1, int* arg2)
{
  return gdk_display_get_window_at_pointer(arg0, arg1, arg2);
}

void Pure_gdk_display_warp_pointer(GdkDisplay* arg0, GdkScreen* arg1, int arg2, int arg3)
{
  return gdk_display_warp_pointer(arg0, arg1, arg2, arg3);
}

GdkDisplayPointerHooks* Pure_gdk_display_set_pointer_hooks(GdkDisplay* arg0, GdkDisplayPointerHooks const* arg1)
{
  return gdk_display_set_pointer_hooks(arg0, arg1);
}

GdkDisplay* Pure_gdk_display_open_default_libgtk_only()
{
  return gdk_display_open_default_libgtk_only();
}

int Pure_gdk_display_supports_cursor_alpha(GdkDisplay* arg0)
{
  return gdk_display_supports_cursor_alpha(arg0);
}

int Pure_gdk_display_supports_cursor_color(GdkDisplay* arg0)
{
  return gdk_display_supports_cursor_color(arg0);
}

unsigned int Pure_gdk_display_get_default_cursor_size(GdkDisplay* arg0)
{
  return gdk_display_get_default_cursor_size(arg0);
}

void Pure_gdk_display_get_maximal_cursor_size(GdkDisplay* arg0, unsigned int* arg1, unsigned int* arg2)
{
  return gdk_display_get_maximal_cursor_size(arg0, arg1, arg2);
}

GdkWindow* Pure_gdk_display_get_default_group(GdkDisplay* arg0)
{
  return gdk_display_get_default_group(arg0);
}

int Pure_gdk_display_supports_selection_notification(GdkDisplay* arg0)
{
  return gdk_display_supports_selection_notification(arg0);
}

int Pure_gdk_display_request_selection_notification(GdkDisplay* arg0, struct _GdkAtom* arg1)
{
  return gdk_display_request_selection_notification(arg0, arg1);
}

int Pure_gdk_display_supports_clipboard_persistence(GdkDisplay* arg0)
{
  return gdk_display_supports_clipboard_persistence(arg0);
}

void Pure_gdk_display_store_clipboard(GdkDisplay* arg0, GdkWindow* arg1, unsigned int arg2, struct _GdkAtom* const* arg3, int arg4)
{
  return gdk_display_store_clipboard(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gdk_display_supports_shapes(GdkDisplay* arg0)
{
  return gdk_display_supports_shapes(arg0);
}

int Pure_gdk_display_supports_input_shapes(GdkDisplay* arg0)
{
  return gdk_display_supports_input_shapes(arg0);
}

int Pure_gdk_display_supports_composite(GdkDisplay* arg0)
{
  return gdk_display_supports_composite(arg0);
}

unsigned long Pure_gdk_screen_get_type()
{
  return gdk_screen_get_type();
}

GdkColormap* Pure_gdk_screen_get_default_colormap(GdkScreen* arg0)
{
  return gdk_screen_get_default_colormap(arg0);
}

void Pure_gdk_screen_set_default_colormap(GdkScreen* arg0, GdkColormap* arg1)
{
  return gdk_screen_set_default_colormap(arg0, arg1);
}

GdkColormap* Pure_gdk_screen_get_system_colormap(GdkScreen* arg0)
{
  return gdk_screen_get_system_colormap(arg0);
}

GdkVisual* Pure_gdk_screen_get_system_visual(GdkScreen* arg0)
{
  return gdk_screen_get_system_visual(arg0);
}

GdkColormap* Pure_gdk_screen_get_rgb_colormap(GdkScreen* arg0)
{
  return gdk_screen_get_rgb_colormap(arg0);
}

GdkVisual* Pure_gdk_screen_get_rgb_visual(GdkScreen* arg0)
{
  return gdk_screen_get_rgb_visual(arg0);
}

GdkColormap* Pure_gdk_screen_get_rgba_colormap(GdkScreen* arg0)
{
  return gdk_screen_get_rgba_colormap(arg0);
}

GdkVisual* Pure_gdk_screen_get_rgba_visual(GdkScreen* arg0)
{
  return gdk_screen_get_rgba_visual(arg0);
}

int Pure_gdk_screen_is_composited(GdkScreen* arg0)
{
  return gdk_screen_is_composited(arg0);
}

GdkWindow* Pure_gdk_screen_get_root_window(GdkScreen* arg0)
{
  return gdk_screen_get_root_window(arg0);
}

GdkDisplay* Pure_gdk_screen_get_display(GdkScreen* arg0)
{
  return gdk_screen_get_display(arg0);
}

int Pure_gdk_screen_get_number(GdkScreen* arg0)
{
  return gdk_screen_get_number(arg0);
}

int Pure_gdk_screen_get_width(GdkScreen* arg0)
{
  return gdk_screen_get_width(arg0);
}

int Pure_gdk_screen_get_height(GdkScreen* arg0)
{
  return gdk_screen_get_height(arg0);
}

int Pure_gdk_screen_get_width_mm(GdkScreen* arg0)
{
  return gdk_screen_get_width_mm(arg0);
}

int Pure_gdk_screen_get_height_mm(GdkScreen* arg0)
{
  return gdk_screen_get_height_mm(arg0);
}

GList* Pure_gdk_screen_list_visuals(GdkScreen* arg0)
{
  return gdk_screen_list_visuals(arg0);
}

GList* Pure_gdk_screen_get_toplevel_windows(GdkScreen* arg0)
{
  return gdk_screen_get_toplevel_windows(arg0);
}

char* Pure_gdk_screen_make_display_name(GdkScreen* arg0)
{
  return gdk_screen_make_display_name(arg0);
}

int Pure_gdk_screen_get_n_monitors(GdkScreen* arg0)
{
  return gdk_screen_get_n_monitors(arg0);
}

void Pure_gdk_screen_get_monitor_geometry(GdkScreen* arg0, int arg1, GdkRectangle* arg2)
{
  return gdk_screen_get_monitor_geometry(arg0, arg1, arg2);
}

int Pure_gdk_screen_get_monitor_at_point(GdkScreen* arg0, int arg1, int arg2)
{
  return gdk_screen_get_monitor_at_point(arg0, arg1, arg2);
}

int Pure_gdk_screen_get_monitor_at_window(GdkScreen* arg0, GdkWindow* arg1)
{
  return gdk_screen_get_monitor_at_window(arg0, arg1);
}

int Pure_gdk_screen_get_monitor_width_mm(GdkScreen* arg0, int arg1)
{
  return gdk_screen_get_monitor_width_mm(arg0, arg1);
}

int Pure_gdk_screen_get_monitor_height_mm(GdkScreen* arg0, int arg1)
{
  return gdk_screen_get_monitor_height_mm(arg0, arg1);
}

char* Pure_gdk_screen_get_monitor_plug_name(GdkScreen* arg0, int arg1)
{
  return gdk_screen_get_monitor_plug_name(arg0, arg1);
}

void Pure_gdk_screen_broadcast_client_message(GdkScreen* arg0, GdkEvent* arg1)
{
  return gdk_screen_broadcast_client_message(arg0, arg1);
}

GdkScreen* Pure_gdk_screen_get_default()
{
  return gdk_screen_get_default();
}

int Pure_gdk_screen_get_setting(GdkScreen* arg0, char const* arg1, GValue* arg2)
{
  return gdk_screen_get_setting(arg0, arg1, arg2);
}

void Pure_gdk_screen_set_font_options(GdkScreen* arg0, cairo_font_options_t const* arg1)
{
  return gdk_screen_set_font_options(arg0, arg1);
}

cairo_font_options_t const* Pure_gdk_screen_get_font_options(GdkScreen* arg0)
{
  return gdk_screen_get_font_options(arg0);
}

void Pure_gdk_screen_set_resolution(GdkScreen* arg0, double arg1)
{
  return gdk_screen_set_resolution(arg0, arg1);
}

double Pure_gdk_screen_get_resolution(GdkScreen* arg0)
{
  return gdk_screen_get_resolution(arg0);
}

GdkWindow* Pure_gdk_screen_get_active_window(GdkScreen* arg0)
{
  return gdk_screen_get_active_window(arg0);
}

GList* Pure_gdk_screen_get_window_stack(GdkScreen* arg0)
{
  return gdk_screen_get_window_stack(arg0);
}

unsigned long Pure_gdk_app_launch_context_get_type()
{
  return gdk_app_launch_context_get_type();
}

GdkAppLaunchContext* Pure_gdk_app_launch_context_new()
{
  return gdk_app_launch_context_new();
}

void Pure_gdk_app_launch_context_set_display(GdkAppLaunchContext* arg0, GdkDisplay* arg1)
{
  return gdk_app_launch_context_set_display(arg0, arg1);
}

void Pure_gdk_app_launch_context_set_screen(GdkAppLaunchContext* arg0, GdkScreen* arg1)
{
  return gdk_app_launch_context_set_screen(arg0, arg1);
}

void Pure_gdk_app_launch_context_set_desktop(GdkAppLaunchContext* arg0, int arg1)
{
  return gdk_app_launch_context_set_desktop(arg0, arg1);
}

void Pure_gdk_app_launch_context_set_timestamp(GdkAppLaunchContext* arg0, unsigned int arg1)
{
  return gdk_app_launch_context_set_timestamp(arg0, arg1);
}

void Pure_gdk_app_launch_context_set_icon(GdkAppLaunchContext* arg0, GIcon* arg1)
{
  return gdk_app_launch_context_set_icon(arg0, arg1);
}

void Pure_gdk_app_launch_context_set_icon_name(GdkAppLaunchContext* arg0, char const* arg1)
{
  return gdk_app_launch_context_set_icon_name(arg0, arg1);
}

void Pure_gdk_rgb_init()
{
  return gdk_rgb_init();
}

unsigned long Pure_gdk_rgb_xpixel_from_rgb(unsigned int arg0)
{
  return gdk_rgb_xpixel_from_rgb(arg0);
}

void Pure_gdk_rgb_gc_set_foreground(GdkGC* arg0, unsigned int arg1)
{
  return gdk_rgb_gc_set_foreground(arg0, arg1);
}

void Pure_gdk_rgb_gc_set_background(GdkGC* arg0, unsigned int arg1)
{
  return gdk_rgb_gc_set_background(arg0, arg1);
}

void Pure_gdk_rgb_find_color(GdkColormap* arg0, GdkColor* arg1)
{
  return gdk_rgb_find_color(arg0, arg1);
}

void Pure_gdk_draw_rgb_image(GdkDrawable* arg0, GdkGC* arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned char const* arg7, int arg8)
{
  return gdk_draw_rgb_image(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gdk_draw_rgb_image_dithalign(GdkDrawable* arg0, GdkGC* arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned char const* arg7, int arg8, int arg9, int arg10)
{
  return gdk_draw_rgb_image_dithalign(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_gdk_draw_rgb_32_image(GdkDrawable* arg0, GdkGC* arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned char const* arg7, int arg8)
{
  return gdk_draw_rgb_32_image(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gdk_draw_rgb_32_image_dithalign(GdkDrawable* arg0, GdkGC* arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned char const* arg7, int arg8, int arg9, int arg10)
{
  return gdk_draw_rgb_32_image_dithalign(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_gdk_draw_gray_image(GdkDrawable* arg0, GdkGC* arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned char const* arg7, int arg8)
{
  return gdk_draw_gray_image(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gdk_draw_indexed_image(GdkDrawable* arg0, GdkGC* arg1, int arg2, int arg3, int arg4, int arg5, unsigned int arg6, unsigned char const* arg7, int arg8, GdkRgbCmap* arg9)
{
  return gdk_draw_indexed_image(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

GdkRgbCmap* Pure_gdk_rgb_cmap_new(unsigned int* arg0, int arg1)
{
  return gdk_rgb_cmap_new(arg0, arg1);
}

void Pure_gdk_rgb_cmap_free(GdkRgbCmap* arg0)
{
  return gdk_rgb_cmap_free(arg0);
}

void Pure_gdk_rgb_set_verbose(int arg0)
{
  return gdk_rgb_set_verbose(arg0);
}

void Pure_gdk_rgb_set_install(int arg0)
{
  return gdk_rgb_set_install(arg0);
}

void Pure_gdk_rgb_set_min_colors(int arg0)
{
  return gdk_rgb_set_min_colors(arg0);
}

GdkColormap* Pure_gdk_rgb_get_colormap()
{
  return gdk_rgb_get_colormap();
}

GdkVisual* Pure_gdk_rgb_get_visual()
{
  return gdk_rgb_get_visual();
}

int Pure_gdk_rgb_ditherable()
{
  return gdk_rgb_ditherable();
}

int Pure_gdk_rgb_colormap_ditherable(GdkColormap* arg0)
{
  return gdk_rgb_colormap_ditherable(arg0);
}

unsigned int Pure_gdk_pixbuf_error_quark()
{
  return gdk_pixbuf_error_quark();
}

unsigned long Pure_gdk_pixbuf_get_type()
{
  return gdk_pixbuf_get_type();
}

GdkPixbuf* Pure_gdk_pixbuf_ref(GdkPixbuf* arg0)
{
  return gdk_pixbuf_ref(arg0);
}

void Pure_gdk_pixbuf_unref(GdkPixbuf* arg0)
{
  return gdk_pixbuf_unref(arg0);
}

unsigned int Pure_gdk_pixbuf_get_colorspace(GdkPixbuf const* arg0)
{
  return gdk_pixbuf_get_colorspace(arg0);
}

int Pure_gdk_pixbuf_get_n_channels(GdkPixbuf const* arg0)
{
  return gdk_pixbuf_get_n_channels(arg0);
}

int Pure_gdk_pixbuf_get_has_alpha(GdkPixbuf const* arg0)
{
  return gdk_pixbuf_get_has_alpha(arg0);
}

int Pure_gdk_pixbuf_get_bits_per_sample(GdkPixbuf const* arg0)
{
  return gdk_pixbuf_get_bits_per_sample(arg0);
}

unsigned char* Pure_gdk_pixbuf_get_pixels(GdkPixbuf const* arg0)
{
  return gdk_pixbuf_get_pixels(arg0);
}

int Pure_gdk_pixbuf_get_width(GdkPixbuf const* arg0)
{
  return gdk_pixbuf_get_width(arg0);
}

int Pure_gdk_pixbuf_get_height(GdkPixbuf const* arg0)
{
  return gdk_pixbuf_get_height(arg0);
}

int Pure_gdk_pixbuf_get_rowstride(GdkPixbuf const* arg0)
{
  return gdk_pixbuf_get_rowstride(arg0);
}

GdkPixbuf* Pure_gdk_pixbuf_new(unsigned int arg0, int arg1, int arg2, int arg3, int arg4)
{
  return gdk_pixbuf_new(arg0, arg1, arg2, arg3, arg4);
}

GdkPixbuf* Pure_gdk_pixbuf_copy(GdkPixbuf const* arg0)
{
  return gdk_pixbuf_copy(arg0);
}

GdkPixbuf* Pure_gdk_pixbuf_new_subpixbuf(GdkPixbuf* arg0, int arg1, int arg2, int arg3, int arg4)
{
  return gdk_pixbuf_new_subpixbuf(arg0, arg1, arg2, arg3, arg4);
}

GdkPixbuf* Pure_gdk_pixbuf_new_from_file(char const* arg0, GError** arg1)
{
  return gdk_pixbuf_new_from_file(arg0, arg1);
}

GdkPixbuf* Pure_gdk_pixbuf_new_from_file_at_size(char const* arg0, int arg1, int arg2, GError** arg3)
{
  return gdk_pixbuf_new_from_file_at_size(arg0, arg1, arg2, arg3);
}

GdkPixbuf* Pure_gdk_pixbuf_new_from_file_at_scale(char const* arg0, int arg1, int arg2, int arg3, GError** arg4)
{
  return gdk_pixbuf_new_from_file_at_scale(arg0, arg1, arg2, arg3, arg4);
}

GdkPixbuf* Pure_gdk_pixbuf_new_from_data(unsigned char const* arg0, unsigned int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, void* arg7, void* arg8)
{
  return gdk_pixbuf_new_from_data(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

GdkPixbuf* Pure_gdk_pixbuf_new_from_xpm_data(char const** arg0)
{
  return gdk_pixbuf_new_from_xpm_data(arg0);
}

GdkPixbuf* Pure_gdk_pixbuf_new_from_inline(int arg0, unsigned char const* arg1, int arg2, GError** arg3)
{
  return gdk_pixbuf_new_from_inline(arg0, arg1, arg2, arg3);
}

void Pure_gdk_pixbuf_fill(GdkPixbuf* arg0, unsigned int arg1)
{
  return gdk_pixbuf_fill(arg0, arg1);
}

int Pure_gdk_pixbuf_save(GdkPixbuf* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return gdk_pixbuf_save(arg0, arg1, arg2, arg3);
}

int Pure_gdk_pixbuf_savev(GdkPixbuf* arg0, char const* arg1, char const* arg2, char** arg3, char** arg4, GError** arg5)
{
  return gdk_pixbuf_savev(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gdk_pixbuf_save_to_callback(GdkPixbuf* arg0, void* arg1, void* arg2, char const* arg3, GError** arg4)
{
  return gdk_pixbuf_save_to_callback(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gdk_pixbuf_save_to_callbackv(GdkPixbuf* arg0, void* arg1, void* arg2, char const* arg3, char** arg4, char** arg5, GError** arg6)
{
  return gdk_pixbuf_save_to_callbackv(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

int Pure_gdk_pixbuf_save_to_buffer(GdkPixbuf* arg0, char** arg1, unsigned long* arg2, char const* arg3, GError** arg4)
{
  return gdk_pixbuf_save_to_buffer(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gdk_pixbuf_save_to_bufferv(GdkPixbuf* arg0, char** arg1, unsigned long* arg2, char const* arg3, char** arg4, char** arg5, GError** arg6)
{
  return gdk_pixbuf_save_to_bufferv(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GdkPixbuf* Pure_gdk_pixbuf_new_from_stream(GInputStream* arg0, GCancellable* arg1, GError** arg2)
{
  return gdk_pixbuf_new_from_stream(arg0, arg1, arg2);
}

GdkPixbuf* Pure_gdk_pixbuf_new_from_stream_at_scale(GInputStream* arg0, int arg1, int arg2, int arg3, GCancellable* arg4, GError** arg5)
{
  return gdk_pixbuf_new_from_stream_at_scale(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gdk_pixbuf_save_to_stream(GdkPixbuf* arg0, GOutputStream* arg1, char const* arg2, GCancellable* arg3, GError** arg4)
{
  return gdk_pixbuf_save_to_stream(arg0, arg1, arg2, arg3, arg4);
}

GdkPixbuf* Pure_gdk_pixbuf_add_alpha(GdkPixbuf const* arg0, int arg1, unsigned char arg2, unsigned char arg3, unsigned char arg4)
{
  return gdk_pixbuf_add_alpha(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gdk_pixbuf_copy_area(GdkPixbuf const* arg0, int arg1, int arg2, int arg3, int arg4, GdkPixbuf* arg5, int arg6, int arg7)
{
  return gdk_pixbuf_copy_area(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gdk_pixbuf_saturate_and_pixelate(GdkPixbuf const* arg0, GdkPixbuf* arg1, float arg2, int arg3)
{
  return gdk_pixbuf_saturate_and_pixelate(arg0, arg1, arg2, arg3);
}

GdkPixbuf* Pure_gdk_pixbuf_apply_embedded_orientation(GdkPixbuf* arg0)
{
  return gdk_pixbuf_apply_embedded_orientation(arg0);
}

char const* Pure_gdk_pixbuf_get_option(GdkPixbuf* arg0, char const* arg1)
{
  return gdk_pixbuf_get_option(arg0, arg1);
}

void Pure_gdk_pixbuf_scale(GdkPixbuf const* arg0, GdkPixbuf* arg1, int arg2, int arg3, int arg4, int arg5, double arg6, double arg7, double arg8, double arg9, unsigned int arg10)
{
  return gdk_pixbuf_scale(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_gdk_pixbuf_composite(GdkPixbuf const* arg0, GdkPixbuf* arg1, int arg2, int arg3, int arg4, int arg5, double arg6, double arg7, double arg8, double arg9, unsigned int arg10, int arg11)
{
  return gdk_pixbuf_composite(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}

void Pure_gdk_pixbuf_composite_color(GdkPixbuf const* arg0, GdkPixbuf* arg1, int arg2, int arg3, int arg4, int arg5, double arg6, double arg7, double arg8, double arg9, unsigned int arg10, int arg11, int arg12, int arg13, int arg14, unsigned int arg15, unsigned int arg16)
{
  return gdk_pixbuf_composite_color(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16);
}

GdkPixbuf* Pure_gdk_pixbuf_scale_simple(GdkPixbuf const* arg0, int arg1, int arg2, unsigned int arg3)
{
  return gdk_pixbuf_scale_simple(arg0, arg1, arg2, arg3);
}

GdkPixbuf* Pure_gdk_pixbuf_composite_color_simple(GdkPixbuf const* arg0, int arg1, int arg2, unsigned int arg3, int arg4, int arg5, unsigned int arg6, unsigned int arg7)
{
  return gdk_pixbuf_composite_color_simple(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

GdkPixbuf* Pure_gdk_pixbuf_rotate_simple(GdkPixbuf const* arg0, unsigned int arg1)
{
  return gdk_pixbuf_rotate_simple(arg0, arg1);
}

GdkPixbuf* Pure_gdk_pixbuf_flip(GdkPixbuf const* arg0, int arg1)
{
  return gdk_pixbuf_flip(arg0, arg1);
}

unsigned long Pure_gdk_pixbuf_animation_get_type()
{
  return gdk_pixbuf_animation_get_type();
}

GdkPixbufAnimation* Pure_gdk_pixbuf_animation_new_from_file(char const* arg0, GError** arg1)
{
  return gdk_pixbuf_animation_new_from_file(arg0, arg1);
}

GdkPixbufAnimation* Pure_gdk_pixbuf_animation_ref(GdkPixbufAnimation* arg0)
{
  return gdk_pixbuf_animation_ref(arg0);
}

void Pure_gdk_pixbuf_animation_unref(GdkPixbufAnimation* arg0)
{
  return gdk_pixbuf_animation_unref(arg0);
}

int Pure_gdk_pixbuf_animation_get_width(GdkPixbufAnimation* arg0)
{
  return gdk_pixbuf_animation_get_width(arg0);
}

int Pure_gdk_pixbuf_animation_get_height(GdkPixbufAnimation* arg0)
{
  return gdk_pixbuf_animation_get_height(arg0);
}

int Pure_gdk_pixbuf_animation_is_static_image(GdkPixbufAnimation* arg0)
{
  return gdk_pixbuf_animation_is_static_image(arg0);
}

GdkPixbuf* Pure_gdk_pixbuf_animation_get_static_image(GdkPixbufAnimation* arg0)
{
  return gdk_pixbuf_animation_get_static_image(arg0);
}

GdkPixbufAnimationIter* Pure_gdk_pixbuf_animation_get_iter(GdkPixbufAnimation* arg0, GTimeVal const* arg1)
{
  return gdk_pixbuf_animation_get_iter(arg0, arg1);
}

unsigned long Pure_gdk_pixbuf_animation_iter_get_type()
{
  return gdk_pixbuf_animation_iter_get_type();
}

int Pure_gdk_pixbuf_animation_iter_get_delay_time(GdkPixbufAnimationIter* arg0)
{
  return gdk_pixbuf_animation_iter_get_delay_time(arg0);
}

GdkPixbuf* Pure_gdk_pixbuf_animation_iter_get_pixbuf(GdkPixbufAnimationIter* arg0)
{
  return gdk_pixbuf_animation_iter_get_pixbuf(arg0);
}

int Pure_gdk_pixbuf_animation_iter_on_currently_loading_frame(GdkPixbufAnimationIter* arg0)
{
  return gdk_pixbuf_animation_iter_on_currently_loading_frame(arg0);
}

int Pure_gdk_pixbuf_animation_iter_advance(GdkPixbufAnimationIter* arg0, GTimeVal const* arg1)
{
  return gdk_pixbuf_animation_iter_advance(arg0, arg1);
}

unsigned long Pure_gdk_pixbuf_simple_anim_get_type()
{
  return gdk_pixbuf_simple_anim_get_type();
}

unsigned long Pure_gdk_pixbuf_simple_anim_iter_get_type()
{
  return gdk_pixbuf_simple_anim_iter_get_type();
}

GdkPixbufSimpleAnim* Pure_gdk_pixbuf_simple_anim_new(int arg0, int arg1, float arg2)
{
  return gdk_pixbuf_simple_anim_new(arg0, arg1, arg2);
}

void Pure_gdk_pixbuf_simple_anim_add_frame(GdkPixbufSimpleAnim* arg0, GdkPixbuf* arg1)
{
  return gdk_pixbuf_simple_anim_add_frame(arg0, arg1);
}

GSList* Pure_gdk_pixbuf_get_formats()
{
  return gdk_pixbuf_get_formats();
}

char* Pure_gdk_pixbuf_format_get_name(GdkPixbufFormat* arg0)
{
  return gdk_pixbuf_format_get_name(arg0);
}

char* Pure_gdk_pixbuf_format_get_description(GdkPixbufFormat* arg0)
{
  return gdk_pixbuf_format_get_description(arg0);
}

char** Pure_gdk_pixbuf_format_get_mime_types(GdkPixbufFormat* arg0)
{
  return gdk_pixbuf_format_get_mime_types(arg0);
}

char** Pure_gdk_pixbuf_format_get_extensions(GdkPixbufFormat* arg0)
{
  return gdk_pixbuf_format_get_extensions(arg0);
}

int Pure_gdk_pixbuf_format_is_writable(GdkPixbufFormat* arg0)
{
  return gdk_pixbuf_format_is_writable(arg0);
}

int Pure_gdk_pixbuf_format_is_scalable(GdkPixbufFormat* arg0)
{
  return gdk_pixbuf_format_is_scalable(arg0);
}

int Pure_gdk_pixbuf_format_is_disabled(GdkPixbufFormat* arg0)
{
  return gdk_pixbuf_format_is_disabled(arg0);
}

void Pure_gdk_pixbuf_format_set_disabled(GdkPixbufFormat* arg0, int arg1)
{
  return gdk_pixbuf_format_set_disabled(arg0, arg1);
}

char* Pure_gdk_pixbuf_format_get_license(GdkPixbufFormat* arg0)
{
  return gdk_pixbuf_format_get_license(arg0);
}

GdkPixbufFormat* Pure_gdk_pixbuf_get_file_info(char const* arg0, int* arg1, int* arg2)
{
  return gdk_pixbuf_get_file_info(arg0, arg1, arg2);
}

unsigned long Pure_gdk_pixbuf_loader_get_type()
{
  return gdk_pixbuf_loader_get_type();
}

GdkPixbufLoader* Pure_gdk_pixbuf_loader_new()
{
  return gdk_pixbuf_loader_new();
}

GdkPixbufLoader* Pure_gdk_pixbuf_loader_new_with_type(char const* arg0, GError** arg1)
{
  return gdk_pixbuf_loader_new_with_type(arg0, arg1);
}

GdkPixbufLoader* Pure_gdk_pixbuf_loader_new_with_mime_type(char const* arg0, GError** arg1)
{
  return gdk_pixbuf_loader_new_with_mime_type(arg0, arg1);
}

void Pure_gdk_pixbuf_loader_set_size(GdkPixbufLoader* arg0, int arg1, int arg2)
{
  return gdk_pixbuf_loader_set_size(arg0, arg1, arg2);
}

int Pure_gdk_pixbuf_loader_write(GdkPixbufLoader* arg0, unsigned char const* arg1, unsigned long arg2, GError** arg3)
{
  return gdk_pixbuf_loader_write(arg0, arg1, arg2, arg3);
}

GdkPixbuf* Pure_gdk_pixbuf_loader_get_pixbuf(GdkPixbufLoader* arg0)
{
  return gdk_pixbuf_loader_get_pixbuf(arg0);
}

GdkPixbufAnimation* Pure_gdk_pixbuf_loader_get_animation(GdkPixbufLoader* arg0)
{
  return gdk_pixbuf_loader_get_animation(arg0);
}

int Pure_gdk_pixbuf_loader_close(GdkPixbufLoader* arg0, GError** arg1)
{
  return gdk_pixbuf_loader_close(arg0, arg1);
}

GdkPixbufFormat* Pure_gdk_pixbuf_loader_get_format(GdkPixbufLoader* arg0)
{
  return gdk_pixbuf_loader_get_format(arg0);
}

unsigned long Pure_gdk_pixbuf_alpha_mode_get_type()
{
  return gdk_pixbuf_alpha_mode_get_type();
}

unsigned long Pure_gdk_colorspace_get_type()
{
  return gdk_colorspace_get_type();
}

unsigned long Pure_gdk_pixbuf_error_get_type()
{
  return gdk_pixbuf_error_get_type();
}

unsigned long Pure_gdk_interp_type_get_type()
{
  return gdk_interp_type_get_type();
}

unsigned long Pure_gdk_pixbuf_rotation_get_type()
{
  return gdk_pixbuf_rotation_get_type();
}

void Pure_gdk_pixbuf_render_threshold_alpha(GdkPixbuf* arg0, GdkBitmap* arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8)
{
  return gdk_pixbuf_render_threshold_alpha(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gdk_pixbuf_render_to_drawable(GdkPixbuf* arg0, GdkDrawable* arg1, GdkGC* arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, unsigned int arg9, int arg10, int arg11)
{
  return gdk_pixbuf_render_to_drawable(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}

void Pure_gdk_pixbuf_render_to_drawable_alpha(GdkPixbuf* arg0, GdkDrawable* arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, int arg9, unsigned int arg10, int arg11, int arg12)
{
  return gdk_pixbuf_render_to_drawable_alpha(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}

void Pure_gdk_pixbuf_render_pixmap_and_mask_for_colormap(GdkPixbuf* arg0, GdkColormap* arg1, GdkPixmap** arg2, GdkBitmap** arg3, int arg4)
{
  return gdk_pixbuf_render_pixmap_and_mask_for_colormap(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gdk_pixbuf_render_pixmap_and_mask(GdkPixbuf* arg0, GdkPixmap** arg1, GdkBitmap** arg2, int arg3)
{
  return gdk_pixbuf_render_pixmap_and_mask(arg0, arg1, arg2, arg3);
}

GdkPixbuf* Pure_gdk_pixbuf_get_from_drawable(GdkPixbuf* arg0, GdkDrawable* arg1, GdkColormap* arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8)
{
  return gdk_pixbuf_get_from_drawable(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

GdkPixbuf* Pure_gdk_pixbuf_get_from_image(GdkPixbuf* arg0, GdkImage* arg1, GdkColormap* arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8)
{
  return gdk_pixbuf_get_from_image(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

cairo_t* Pure_gdk_cairo_create(GdkDrawable* arg0)
{
  return gdk_cairo_create(arg0);
}

void Pure_gdk_cairo_set_source_color(cairo_t* arg0, GdkColor const* arg1)
{
  return gdk_cairo_set_source_color(arg0, arg1);
}

void Pure_gdk_cairo_set_source_pixbuf(cairo_t* arg0, GdkPixbuf const* arg1, double arg2, double arg3)
{
  return gdk_cairo_set_source_pixbuf(arg0, arg1, arg2, arg3);
}

void Pure_gdk_cairo_set_source_pixmap(cairo_t* arg0, GdkPixmap* arg1, double arg2, double arg3)
{
  return gdk_cairo_set_source_pixmap(arg0, arg1, arg2, arg3);
}

void Pure_gdk_cairo_rectangle(cairo_t* arg0, GdkRectangle const* arg1)
{
  return gdk_cairo_rectangle(arg0, arg1);
}

void Pure_gdk_cairo_region(cairo_t* arg0, GdkRegion const* arg1)
{
  return gdk_cairo_region(arg0, arg1);
}

unsigned long Pure_gdk_cursor_get_type()
{
  return gdk_cursor_get_type();
}

GdkCursor* Pure_gdk_cursor_new_for_display(GdkDisplay* arg0, int arg1)
{
  return gdk_cursor_new_for_display(arg0, arg1);
}

GdkCursor* Pure_gdk_cursor_new(int arg0)
{
  return gdk_cursor_new(arg0);
}

GdkCursor* Pure_gdk_cursor_new_from_pixmap(GdkPixmap* arg0, GdkPixmap* arg1, GdkColor const* arg2, GdkColor const* arg3, int arg4, int arg5)
{
  return gdk_cursor_new_from_pixmap(arg0, arg1, arg2, arg3, arg4, arg5);
}

GdkCursor* Pure_gdk_cursor_new_from_pixbuf(GdkDisplay* arg0, GdkPixbuf* arg1, int arg2, int arg3)
{
  return gdk_cursor_new_from_pixbuf(arg0, arg1, arg2, arg3);
}

GdkDisplay* Pure_gdk_cursor_get_display(GdkCursor* arg0)
{
  return gdk_cursor_get_display(arg0);
}

GdkCursor* Pure_gdk_cursor_ref(GdkCursor* arg0)
{
  return gdk_cursor_ref(arg0);
}

void Pure_gdk_cursor_unref(GdkCursor* arg0)
{
  return gdk_cursor_unref(arg0);
}

GdkCursor* Pure_gdk_cursor_new_from_name(GdkDisplay* arg0, char const* arg1)
{
  return gdk_cursor_new_from_name(arg0, arg1);
}

GdkPixbuf* Pure_gdk_cursor_get_image(GdkCursor* arg0)
{
  return gdk_cursor_get_image(arg0);
}

unsigned long Pure_gdk_display_manager_get_type()
{
  return gdk_display_manager_get_type();
}

GdkDisplayManager* Pure_gdk_display_manager_get()
{
  return gdk_display_manager_get();
}

GdkDisplay* Pure_gdk_display_manager_get_default_display(GdkDisplayManager* arg0)
{
  return gdk_display_manager_get_default_display(arg0);
}

void Pure_gdk_display_manager_set_default_display(GdkDisplayManager* arg0, GdkDisplay* arg1)
{
  return gdk_display_manager_set_default_display(arg0, arg1);
}

GSList* Pure_gdk_display_manager_list_displays(GdkDisplayManager* arg0)
{
  return gdk_display_manager_list_displays(arg0);
}

unsigned long Pure_gdk_gc_get_type()
{
  return gdk_gc_get_type();
}

GdkGC* Pure_gdk_gc_new(GdkDrawable* arg0)
{
  return gdk_gc_new(arg0);
}

GdkGC* Pure_gdk_gc_new_with_values(GdkDrawable* arg0, GdkGCValues* arg1, unsigned int arg2)
{
  return gdk_gc_new_with_values(arg0, arg1, arg2);
}

GdkGC* Pure_gdk_gc_ref(GdkGC* arg0)
{
  return gdk_gc_ref(arg0);
}

void Pure_gdk_gc_unref(GdkGC* arg0)
{
  return gdk_gc_unref(arg0);
}

void Pure_gdk_gc_get_values(GdkGC* arg0, GdkGCValues* arg1)
{
  return gdk_gc_get_values(arg0, arg1);
}

void Pure_gdk_gc_set_values(GdkGC* arg0, GdkGCValues* arg1, unsigned int arg2)
{
  return gdk_gc_set_values(arg0, arg1, arg2);
}

void Pure_gdk_gc_set_foreground(GdkGC* arg0, GdkColor const* arg1)
{
  return gdk_gc_set_foreground(arg0, arg1);
}

void Pure_gdk_gc_set_background(GdkGC* arg0, GdkColor const* arg1)
{
  return gdk_gc_set_background(arg0, arg1);
}

void Pure_gdk_gc_set_font(GdkGC* arg0, GdkFont* arg1)
{
  return gdk_gc_set_font(arg0, arg1);
}

void Pure_gdk_gc_set_function(GdkGC* arg0, unsigned int arg1)
{
  return gdk_gc_set_function(arg0, arg1);
}

void Pure_gdk_gc_set_fill(GdkGC* arg0, unsigned int arg1)
{
  return gdk_gc_set_fill(arg0, arg1);
}

void Pure_gdk_gc_set_tile(GdkGC* arg0, GdkPixmap* arg1)
{
  return gdk_gc_set_tile(arg0, arg1);
}

void Pure_gdk_gc_set_stipple(GdkGC* arg0, GdkPixmap* arg1)
{
  return gdk_gc_set_stipple(arg0, arg1);
}

void Pure_gdk_gc_set_ts_origin(GdkGC* arg0, int arg1, int arg2)
{
  return gdk_gc_set_ts_origin(arg0, arg1, arg2);
}

void Pure_gdk_gc_set_clip_origin(GdkGC* arg0, int arg1, int arg2)
{
  return gdk_gc_set_clip_origin(arg0, arg1, arg2);
}

void Pure_gdk_gc_set_clip_mask(GdkGC* arg0, GdkBitmap* arg1)
{
  return gdk_gc_set_clip_mask(arg0, arg1);
}

void Pure_gdk_gc_set_clip_rectangle(GdkGC* arg0, GdkRectangle const* arg1)
{
  return gdk_gc_set_clip_rectangle(arg0, arg1);
}

void Pure_gdk_gc_set_clip_region(GdkGC* arg0, GdkRegion const* arg1)
{
  return gdk_gc_set_clip_region(arg0, arg1);
}

void Pure_gdk_gc_set_subwindow(GdkGC* arg0, unsigned int arg1)
{
  return gdk_gc_set_subwindow(arg0, arg1);
}

void Pure_gdk_gc_set_exposures(GdkGC* arg0, int arg1)
{
  return gdk_gc_set_exposures(arg0, arg1);
}

void Pure_gdk_gc_set_line_attributes(GdkGC* arg0, int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4)
{
  return gdk_gc_set_line_attributes(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gdk_gc_set_dashes(GdkGC* arg0, int arg1, char* arg2, int arg3)
{
  return gdk_gc_set_dashes(arg0, arg1, arg2, arg3);
}

void Pure_gdk_gc_offset(GdkGC* arg0, int arg1, int arg2)
{
  return gdk_gc_offset(arg0, arg1, arg2);
}

void Pure_gdk_gc_copy(GdkGC* arg0, GdkGC* arg1)
{
  return gdk_gc_copy(arg0, arg1);
}

void Pure_gdk_gc_set_colormap(GdkGC* arg0, GdkColormap* arg1)
{
  return gdk_gc_set_colormap(arg0, arg1);
}

GdkColormap* Pure_gdk_gc_get_colormap(GdkGC* arg0)
{
  return gdk_gc_get_colormap(arg0);
}

void Pure_gdk_gc_set_rgb_fg_color(GdkGC* arg0, GdkColor const* arg1)
{
  return gdk_gc_set_rgb_fg_color(arg0, arg1);
}

void Pure_gdk_gc_set_rgb_bg_color(GdkGC* arg0, GdkColor const* arg1)
{
  return gdk_gc_set_rgb_bg_color(arg0, arg1);
}

GdkScreen* Pure_gdk_gc_get_screen(GdkGC* arg0)
{
  return gdk_gc_get_screen(arg0);
}

unsigned long Pure_gdk_drawable_get_type()
{
  return gdk_drawable_get_type();
}

void Pure_gdk_drawable_set_data(GdkDrawable* arg0, char const* arg1, void* arg2, void* arg3)
{
  return gdk_drawable_set_data(arg0, arg1, arg2, arg3);
}

void* Pure_gdk_drawable_get_data(GdkDrawable* arg0, char const* arg1)
{
  return gdk_drawable_get_data(arg0, arg1);
}

void Pure_gdk_drawable_get_size(GdkDrawable* arg0, int* arg1, int* arg2)
{
  return gdk_drawable_get_size(arg0, arg1, arg2);
}

void Pure_gdk_drawable_set_colormap(GdkDrawable* arg0, GdkColormap* arg1)
{
  return gdk_drawable_set_colormap(arg0, arg1);
}

GdkColormap* Pure_gdk_drawable_get_colormap(GdkDrawable* arg0)
{
  return gdk_drawable_get_colormap(arg0);
}

GdkVisual* Pure_gdk_drawable_get_visual(GdkDrawable* arg0)
{
  return gdk_drawable_get_visual(arg0);
}

int Pure_gdk_drawable_get_depth(GdkDrawable* arg0)
{
  return gdk_drawable_get_depth(arg0);
}

GdkScreen* Pure_gdk_drawable_get_screen(GdkDrawable* arg0)
{
  return gdk_drawable_get_screen(arg0);
}

GdkDisplay* Pure_gdk_drawable_get_display(GdkDrawable* arg0)
{
  return gdk_drawable_get_display(arg0);
}

GdkDrawable* Pure_gdk_drawable_ref(GdkDrawable* arg0)
{
  return gdk_drawable_ref(arg0);
}

void Pure_gdk_drawable_unref(GdkDrawable* arg0)
{
  return gdk_drawable_unref(arg0);
}

void Pure_gdk_draw_point(GdkDrawable* arg0, GdkGC* arg1, int arg2, int arg3)
{
  return gdk_draw_point(arg0, arg1, arg2, arg3);
}

void Pure_gdk_draw_line(GdkDrawable* arg0, GdkGC* arg1, int arg2, int arg3, int arg4, int arg5)
{
  return gdk_draw_line(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gdk_draw_rectangle(GdkDrawable* arg0, GdkGC* arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{
  return gdk_draw_rectangle(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gdk_draw_arc(GdkDrawable* arg0, GdkGC* arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8)
{
  return gdk_draw_arc(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gdk_draw_polygon(GdkDrawable* arg0, GdkGC* arg1, int arg2, GdkPoint const* arg3, int arg4)
{
  return gdk_draw_polygon(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gdk_draw_string(GdkDrawable* arg0, GdkFont* arg1, GdkGC* arg2, int arg3, int arg4, char const* arg5)
{
  return gdk_draw_string(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gdk_draw_text(GdkDrawable* arg0, GdkFont* arg1, GdkGC* arg2, int arg3, int arg4, char const* arg5, int arg6)
{
  return gdk_draw_text(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gdk_draw_text_wc(GdkDrawable* arg0, GdkFont* arg1, GdkGC* arg2, int arg3, int arg4, unsigned int const* arg5, int arg6)
{
  return gdk_draw_text_wc(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gdk_draw_drawable(GdkDrawable* arg0, GdkGC* arg1, GdkDrawable* arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8)
{
  return gdk_draw_drawable(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gdk_draw_image(GdkDrawable* arg0, GdkGC* arg1, GdkImage* arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8)
{
  return gdk_draw_image(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gdk_draw_points(GdkDrawable* arg0, GdkGC* arg1, GdkPoint const* arg2, int arg3)
{
  return gdk_draw_points(arg0, arg1, arg2, arg3);
}

void Pure_gdk_draw_segments(GdkDrawable* arg0, GdkGC* arg1, GdkSegment const* arg2, int arg3)
{
  return gdk_draw_segments(arg0, arg1, arg2, arg3);
}

void Pure_gdk_draw_lines(GdkDrawable* arg0, GdkGC* arg1, GdkPoint const* arg2, int arg3)
{
  return gdk_draw_lines(arg0, arg1, arg2, arg3);
}

void Pure_gdk_draw_pixbuf(GdkDrawable* arg0, GdkGC* arg1, GdkPixbuf const* arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, unsigned int arg9, int arg10, int arg11)
{
  return gdk_draw_pixbuf(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}

void Pure_gdk_draw_glyphs(GdkDrawable* arg0, GdkGC* arg1, PangoFont* arg2, int arg3, int arg4, PangoGlyphString* arg5)
{
  return gdk_draw_glyphs(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gdk_draw_layout_line(GdkDrawable* arg0, GdkGC* arg1, int arg2, int arg3, PangoLayoutLine* arg4)
{
  return gdk_draw_layout_line(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gdk_draw_layout(GdkDrawable* arg0, GdkGC* arg1, int arg2, int arg3, PangoLayout* arg4)
{
  return gdk_draw_layout(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gdk_draw_layout_line_with_colors(GdkDrawable* arg0, GdkGC* arg1, int arg2, int arg3, PangoLayoutLine* arg4, GdkColor const* arg5, GdkColor const* arg6)
{
  return gdk_draw_layout_line_with_colors(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gdk_draw_layout_with_colors(GdkDrawable* arg0, GdkGC* arg1, int arg2, int arg3, PangoLayout* arg4, GdkColor const* arg5, GdkColor const* arg6)
{
  return gdk_draw_layout_with_colors(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gdk_draw_glyphs_transformed(GdkDrawable* arg0, GdkGC* arg1, PangoMatrix const* arg2, PangoFont* arg3, int arg4, int arg5, PangoGlyphString* arg6)
{
  return gdk_draw_glyphs_transformed(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gdk_draw_trapezoids(GdkDrawable* arg0, GdkGC* arg1, GdkTrapezoid const* arg2, int arg3)
{
  return gdk_draw_trapezoids(arg0, arg1, arg2, arg3);
}

GdkImage* Pure_gdk_drawable_get_image(GdkDrawable* arg0, int arg1, int arg2, int arg3, int arg4)
{
  return gdk_drawable_get_image(arg0, arg1, arg2, arg3, arg4);
}

GdkImage* Pure_gdk_drawable_copy_to_image(GdkDrawable* arg0, GdkImage* arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7)
{
  return gdk_drawable_copy_to_image(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

GdkRegion* Pure_gdk_drawable_get_clip_region(GdkDrawable* arg0)
{
  return gdk_drawable_get_clip_region(arg0);
}

GdkRegion* Pure_gdk_drawable_get_visible_region(GdkDrawable* arg0)
{
  return gdk_drawable_get_visible_region(arg0);
}

unsigned long Pure_gdk_cursor_type_get_type()
{
  return gdk_cursor_type_get_type();
}

unsigned long Pure_gdk_drag_action_get_type()
{
  return gdk_drag_action_get_type();
}

unsigned long Pure_gdk_drag_protocol_get_type()
{
  return gdk_drag_protocol_get_type();
}

unsigned long Pure_gdk_filter_return_get_type()
{
  return gdk_filter_return_get_type();
}

unsigned long Pure_gdk_event_type_get_type()
{
  return gdk_event_type_get_type();
}

unsigned long Pure_gdk_event_mask_get_type()
{
  return gdk_event_mask_get_type();
}

unsigned long Pure_gdk_visibility_state_get_type()
{
  return gdk_visibility_state_get_type();
}

unsigned long Pure_gdk_scroll_direction_get_type()
{
  return gdk_scroll_direction_get_type();
}

unsigned long Pure_gdk_notify_type_get_type()
{
  return gdk_notify_type_get_type();
}

unsigned long Pure_gdk_crossing_mode_get_type()
{
  return gdk_crossing_mode_get_type();
}

unsigned long Pure_gdk_property_state_get_type()
{
  return gdk_property_state_get_type();
}

unsigned long Pure_gdk_window_state_get_type()
{
  return gdk_window_state_get_type();
}

unsigned long Pure_gdk_setting_action_get_type()
{
  return gdk_setting_action_get_type();
}

unsigned long Pure_gdk_owner_change_get_type()
{
  return gdk_owner_change_get_type();
}

unsigned long Pure_gdk_font_type_get_type()
{
  return gdk_font_type_get_type();
}

unsigned long Pure_gdk_cap_style_get_type()
{
  return gdk_cap_style_get_type();
}

unsigned long Pure_gdk_fill_get_type()
{
  return gdk_fill_get_type();
}

unsigned long Pure_gdk_function_get_type()
{
  return gdk_function_get_type();
}

unsigned long Pure_gdk_join_style_get_type()
{
  return gdk_join_style_get_type();
}

unsigned long Pure_gdk_line_style_get_type()
{
  return gdk_line_style_get_type();
}

unsigned long Pure_gdk_subwindow_mode_get_type()
{
  return gdk_subwindow_mode_get_type();
}

unsigned long Pure_gdk_gc_values_mask_get_type()
{
  return gdk_gc_values_mask_get_type();
}

unsigned long Pure_gdk_image_type_get_type()
{
  return gdk_image_type_get_type();
}

unsigned long Pure_gdk_extension_mode_get_type()
{
  return gdk_extension_mode_get_type();
}

unsigned long Pure_gdk_input_source_get_type()
{
  return gdk_input_source_get_type();
}

unsigned long Pure_gdk_input_mode_get_type()
{
  return gdk_input_mode_get_type();
}

unsigned long Pure_gdk_axis_use_get_type()
{
  return gdk_axis_use_get_type();
}

unsigned long Pure_gdk_prop_mode_get_type()
{
  return gdk_prop_mode_get_type();
}

unsigned long Pure_gdk_fill_rule_get_type()
{
  return gdk_fill_rule_get_type();
}

unsigned long Pure_gdk_overlap_type_get_type()
{
  return gdk_overlap_type_get_type();
}

unsigned long Pure_gdk_rgb_dither_get_type()
{
  return gdk_rgb_dither_get_type();
}

unsigned long Pure_gdk_byte_order_get_type()
{
  return gdk_byte_order_get_type();
}

unsigned long Pure_gdk_modifier_type_get_type()
{
  return gdk_modifier_type_get_type();
}

unsigned long Pure_gdk_input_condition_get_type()
{
  return gdk_input_condition_get_type();
}

unsigned long Pure_gdk_status_get_type()
{
  return gdk_status_get_type();
}

unsigned long Pure_gdk_grab_status_get_type()
{
  return gdk_grab_status_get_type();
}

unsigned long Pure_gdk_visual_type_get_type()
{
  return gdk_visual_type_get_type();
}

unsigned long Pure_gdk_window_class_get_type()
{
  return gdk_window_class_get_type();
}

unsigned long Pure_gdk_window_type_get_type()
{
  return gdk_window_type_get_type();
}

unsigned long Pure_gdk_window_attributes_type_get_type()
{
  return gdk_window_attributes_type_get_type();
}

unsigned long Pure_gdk_window_hints_get_type()
{
  return gdk_window_hints_get_type();
}

unsigned long Pure_gdk_window_type_hint_get_type()
{
  return gdk_window_type_hint_get_type();
}

unsigned long Pure_gdk_wm_decoration_get_type()
{
  return gdk_wm_decoration_get_type();
}

unsigned long Pure_gdk_wm_function_get_type()
{
  return gdk_wm_function_get_type();
}

unsigned long Pure_gdk_gravity_get_type()
{
  return gdk_gravity_get_type();
}

unsigned long Pure_gdk_window_edge_get_type()
{
  return gdk_window_edge_get_type();
}

unsigned long Pure_gdk_font_get_type()
{
  return gdk_font_get_type();
}

GdkFont* Pure_gdk_font_ref(GdkFont* arg0)
{
  return gdk_font_ref(arg0);
}

void Pure_gdk_font_unref(GdkFont* arg0)
{
  return gdk_font_unref(arg0);
}

int Pure_gdk_font_id(GdkFont const* arg0)
{
  return gdk_font_id(arg0);
}

int Pure_gdk_font_equal(GdkFont const* arg0, GdkFont const* arg1)
{
  return gdk_font_equal(arg0, arg1);
}

GdkFont* Pure_gdk_font_load_for_display(GdkDisplay* arg0, char const* arg1)
{
  return gdk_font_load_for_display(arg0, arg1);
}

GdkFont* Pure_gdk_fontset_load_for_display(GdkDisplay* arg0, char const* arg1)
{
  return gdk_fontset_load_for_display(arg0, arg1);
}

GdkFont* Pure_gdk_font_from_description_for_display(GdkDisplay* arg0, PangoFontDescription* arg1)
{
  return gdk_font_from_description_for_display(arg0, arg1);
}

GdkFont* Pure_gdk_font_load(char const* arg0)
{
  return gdk_font_load(arg0);
}

GdkFont* Pure_gdk_fontset_load(char const* arg0)
{
  return gdk_fontset_load(arg0);
}

GdkFont* Pure_gdk_font_from_description(PangoFontDescription* arg0)
{
  return gdk_font_from_description(arg0);
}

int Pure_gdk_string_width(GdkFont* arg0, char const* arg1)
{
  return gdk_string_width(arg0, arg1);
}

int Pure_gdk_text_width(GdkFont* arg0, char const* arg1, int arg2)
{
  return gdk_text_width(arg0, arg1, arg2);
}

int Pure_gdk_text_width_wc(GdkFont* arg0, unsigned int const* arg1, int arg2)
{
  return gdk_text_width_wc(arg0, arg1, arg2);
}

int Pure_gdk_char_width(GdkFont* arg0, char arg1)
{
  return gdk_char_width(arg0, arg1);
}

int Pure_gdk_char_width_wc(GdkFont* arg0, unsigned int arg1)
{
  return gdk_char_width_wc(arg0, arg1);
}

int Pure_gdk_string_measure(GdkFont* arg0, char const* arg1)
{
  return gdk_string_measure(arg0, arg1);
}

int Pure_gdk_text_measure(GdkFont* arg0, char const* arg1, int arg2)
{
  return gdk_text_measure(arg0, arg1, arg2);
}

int Pure_gdk_char_measure(GdkFont* arg0, char arg1)
{
  return gdk_char_measure(arg0, arg1);
}

int Pure_gdk_string_height(GdkFont* arg0, char const* arg1)
{
  return gdk_string_height(arg0, arg1);
}

int Pure_gdk_text_height(GdkFont* arg0, char const* arg1, int arg2)
{
  return gdk_text_height(arg0, arg1, arg2);
}

int Pure_gdk_char_height(GdkFont* arg0, char arg1)
{
  return gdk_char_height(arg0, arg1);
}

void Pure_gdk_text_extents(GdkFont* arg0, char const* arg1, int arg2, int* arg3, int* arg4, int* arg5, int* arg6, int* arg7)
{
  return gdk_text_extents(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gdk_text_extents_wc(GdkFont* arg0, unsigned int const* arg1, int arg2, int* arg3, int* arg4, int* arg5, int* arg6, int* arg7)
{
  return gdk_text_extents_wc(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gdk_string_extents(GdkFont* arg0, char const* arg1, int* arg2, int* arg3, int* arg4, int* arg5, int* arg6)
{
  return gdk_string_extents(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GdkDisplay* Pure_gdk_font_get_display(GdkFont* arg0)
{
  return gdk_font_get_display(arg0);
}

unsigned long Pure_gdk_image_get_type()
{
  return gdk_image_get_type();
}

GdkImage* Pure_gdk_image_new(unsigned int arg0, GdkVisual* arg1, int arg2, int arg3)
{
  return gdk_image_new(arg0, arg1, arg2, arg3);
}

GdkImage* Pure_gdk_image_get(GdkDrawable* arg0, int arg1, int arg2, int arg3, int arg4)
{
  return gdk_image_get(arg0, arg1, arg2, arg3, arg4);
}

GdkImage* Pure_gdk_image_ref(GdkImage* arg0)
{
  return gdk_image_ref(arg0);
}

void Pure_gdk_image_unref(GdkImage* arg0)
{
  return gdk_image_unref(arg0);
}

void Pure_gdk_image_put_pixel(GdkImage* arg0, int arg1, int arg2, unsigned int arg3)
{
  return gdk_image_put_pixel(arg0, arg1, arg2, arg3);
}

unsigned int Pure_gdk_image_get_pixel(GdkImage* arg0, int arg1, int arg2)
{
  return gdk_image_get_pixel(arg0, arg1, arg2);
}

void Pure_gdk_image_set_colormap(GdkImage* arg0, GdkColormap* arg1)
{
  return gdk_image_set_colormap(arg0, arg1);
}

GdkColormap* Pure_gdk_image_get_colormap(GdkImage* arg0)
{
  return gdk_image_get_colormap(arg0);
}

unsigned long Pure_gdk_keymap_get_type()
{
  return gdk_keymap_get_type();
}

GdkKeymap* Pure_gdk_keymap_get_default()
{
  return gdk_keymap_get_default();
}

GdkKeymap* Pure_gdk_keymap_get_for_display(GdkDisplay* arg0)
{
  return gdk_keymap_get_for_display(arg0);
}

unsigned int Pure_gdk_keymap_lookup_key(GdkKeymap* arg0, GdkKeymapKey const* arg1)
{
  return gdk_keymap_lookup_key(arg0, arg1);
}

int Pure_gdk_keymap_translate_keyboard_state(GdkKeymap* arg0, unsigned int arg1, unsigned int arg2, int arg3, unsigned int* arg4, int* arg5, int* arg6, unsigned int* arg7)
{
  return gdk_keymap_translate_keyboard_state(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

int Pure_gdk_keymap_get_entries_for_keyval(GdkKeymap* arg0, unsigned int arg1, GdkKeymapKey** arg2, int* arg3)
{
  return gdk_keymap_get_entries_for_keyval(arg0, arg1, arg2, arg3);
}

int Pure_gdk_keymap_get_entries_for_keycode(GdkKeymap* arg0, unsigned int arg1, GdkKeymapKey** arg2, unsigned int** arg3, int* arg4)
{
  return gdk_keymap_get_entries_for_keycode(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_gdk_keymap_get_direction(GdkKeymap* arg0)
{
  return gdk_keymap_get_direction(arg0);
}

int Pure_gdk_keymap_have_bidi_layouts(GdkKeymap* arg0)
{
  return gdk_keymap_have_bidi_layouts(arg0);
}

char* Pure_gdk_keyval_name(unsigned int arg0)
{
  return gdk_keyval_name(arg0);
}

unsigned int Pure_gdk_keyval_from_name(char const* arg0)
{
  return gdk_keyval_from_name(arg0);
}

void Pure_gdk_keyval_convert_case(unsigned int arg0, unsigned int* arg1, unsigned int* arg2)
{
  return gdk_keyval_convert_case(arg0, arg1, arg2);
}

unsigned int Pure_gdk_keyval_to_upper(unsigned int arg0)
{
  return gdk_keyval_to_upper(arg0);
}

unsigned int Pure_gdk_keyval_to_lower(unsigned int arg0)
{
  return gdk_keyval_to_lower(arg0);
}

int Pure_gdk_keyval_is_upper(unsigned int arg0)
{
  return gdk_keyval_is_upper(arg0);
}

int Pure_gdk_keyval_is_lower(unsigned int arg0)
{
  return gdk_keyval_is_lower(arg0);
}

unsigned int Pure_gdk_keyval_to_unicode(unsigned int arg0)
{
  return gdk_keyval_to_unicode(arg0);
}

unsigned int Pure_gdk_unicode_to_keyval(unsigned int arg0)
{
  return gdk_unicode_to_keyval(arg0);
}

unsigned long Pure_gdk_pango_renderer_get_type()
{
  return gdk_pango_renderer_get_type();
}

PangoRenderer* Pure_gdk_pango_renderer_new(GdkScreen* arg0)
{
  return gdk_pango_renderer_new(arg0);
}

PangoRenderer* Pure_gdk_pango_renderer_get_default(GdkScreen* arg0)
{
  return gdk_pango_renderer_get_default(arg0);
}

void Pure_gdk_pango_renderer_set_drawable(GdkPangoRenderer* arg0, GdkDrawable* arg1)
{
  return gdk_pango_renderer_set_drawable(arg0, arg1);
}

void Pure_gdk_pango_renderer_set_gc(GdkPangoRenderer* arg0, GdkGC* arg1)
{
  return gdk_pango_renderer_set_gc(arg0, arg1);
}

void Pure_gdk_pango_renderer_set_stipple(GdkPangoRenderer* arg0, unsigned int arg1, GdkBitmap* arg2)
{
  return gdk_pango_renderer_set_stipple(arg0, arg1, arg2);
}

void Pure_gdk_pango_renderer_set_override_color(GdkPangoRenderer* arg0, unsigned int arg1, GdkColor const* arg2)
{
  return gdk_pango_renderer_set_override_color(arg0, arg1, arg2);
}

PangoContext* Pure_gdk_pango_context_get_for_screen(GdkScreen* arg0)
{
  return gdk_pango_context_get_for_screen(arg0);
}

PangoContext* Pure_gdk_pango_context_get()
{
  return gdk_pango_context_get();
}

void Pure_gdk_pango_context_set_colormap(PangoContext* arg0, GdkColormap* arg1)
{
  return gdk_pango_context_set_colormap(arg0, arg1);
}

GdkRegion* Pure_gdk_pango_layout_line_get_clip_region(PangoLayoutLine* arg0, int arg1, int arg2, int const* arg3, int arg4)
{
  return gdk_pango_layout_line_get_clip_region(arg0, arg1, arg2, arg3, arg4);
}

GdkRegion* Pure_gdk_pango_layout_get_clip_region(PangoLayout* arg0, int arg1, int arg2, int const* arg3, int arg4)
{
  return gdk_pango_layout_get_clip_region(arg0, arg1, arg2, arg3, arg4);
}

PangoAttribute* Pure_gdk_pango_attr_stipple_new(GdkBitmap* arg0)
{
  return gdk_pango_attr_stipple_new(arg0);
}

PangoAttribute* Pure_gdk_pango_attr_embossed_new(int arg0)
{
  return gdk_pango_attr_embossed_new(arg0);
}

PangoAttribute* Pure_gdk_pango_attr_emboss_color_new(GdkColor const* arg0)
{
  return gdk_pango_attr_emboss_color_new(arg0);
}

unsigned long Pure_gdk_pixmap_get_type()
{
  return gdk_pixmap_get_type();
}

GdkPixmap* Pure_gdk_pixmap_new(GdkDrawable* arg0, int arg1, int arg2, int arg3)
{
  return gdk_pixmap_new(arg0, arg1, arg2, arg3);
}

GdkBitmap* Pure_gdk_bitmap_create_from_data(GdkDrawable* arg0, char const* arg1, int arg2, int arg3)
{
  return gdk_bitmap_create_from_data(arg0, arg1, arg2, arg3);
}

GdkPixmap* Pure_gdk_pixmap_create_from_data(GdkDrawable* arg0, char const* arg1, int arg2, int arg3, int arg4, GdkColor const* arg5, GdkColor const* arg6)
{
  return gdk_pixmap_create_from_data(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GdkPixmap* Pure_gdk_pixmap_create_from_xpm(GdkDrawable* arg0, GdkBitmap** arg1, GdkColor const* arg2, char const* arg3)
{
  return gdk_pixmap_create_from_xpm(arg0, arg1, arg2, arg3);
}

GdkPixmap* Pure_gdk_pixmap_colormap_create_from_xpm(GdkDrawable* arg0, GdkColormap* arg1, GdkBitmap** arg2, GdkColor const* arg3, char const* arg4)
{
  return gdk_pixmap_colormap_create_from_xpm(arg0, arg1, arg2, arg3, arg4);
}

GdkPixmap* Pure_gdk_pixmap_create_from_xpm_d(GdkDrawable* arg0, GdkBitmap** arg1, GdkColor const* arg2, char** arg3)
{
  return gdk_pixmap_create_from_xpm_d(arg0, arg1, arg2, arg3);
}

GdkPixmap* Pure_gdk_pixmap_colormap_create_from_xpm_d(GdkDrawable* arg0, GdkColormap* arg1, GdkBitmap** arg2, GdkColor const* arg3, char** arg4)
{
  return gdk_pixmap_colormap_create_from_xpm_d(arg0, arg1, arg2, arg3, arg4);
}

GdkPixmap* Pure_gdk_pixmap_foreign_new(unsigned int arg0)
{
  return gdk_pixmap_foreign_new(arg0);
}

GdkPixmap* Pure_gdk_pixmap_lookup(unsigned int arg0)
{
  return gdk_pixmap_lookup(arg0);
}

GdkPixmap* Pure_gdk_pixmap_foreign_new_for_display(GdkDisplay* arg0, unsigned int arg1)
{
  return gdk_pixmap_foreign_new_for_display(arg0, arg1);
}

GdkPixmap* Pure_gdk_pixmap_lookup_for_display(GdkDisplay* arg0, unsigned int arg1)
{
  return gdk_pixmap_lookup_for_display(arg0, arg1);
}

GdkPixmap* Pure_gdk_pixmap_foreign_new_for_screen(GdkScreen* arg0, unsigned int arg1, int arg2, int arg3, int arg4)
{
  return gdk_pixmap_foreign_new_for_screen(arg0, arg1, arg2, arg3, arg4);
}

struct _GdkAtom* Pure_gdk_atom_intern(char const* arg0, int arg1)
{
  return gdk_atom_intern(arg0, arg1);
}

struct _GdkAtom* Pure_gdk_atom_intern_static_string(char const* arg0)
{
  return gdk_atom_intern_static_string(arg0);
}

char* Pure_gdk_atom_name(struct _GdkAtom* arg0)
{
  return gdk_atom_name(arg0);
}

int Pure_gdk_property_get(GdkWindow* arg0, struct _GdkAtom* arg1, struct _GdkAtom* arg2, unsigned long arg3, unsigned long arg4, int arg5, struct _GdkAtom** arg6, int* arg7, int* arg8, unsigned char** arg9)
{
  return gdk_property_get(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_gdk_property_change(GdkWindow* arg0, struct _GdkAtom* arg1, struct _GdkAtom* arg2, int arg3, unsigned int arg4, unsigned char const* arg5, int arg6)
{
  return gdk_property_change(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gdk_property_delete(GdkWindow* arg0, struct _GdkAtom* arg1)
{
  return gdk_property_delete(arg0, arg1);
}

int Pure_gdk_text_property_to_text_list(struct _GdkAtom* arg0, int arg1, unsigned char const* arg2, int arg3, char*** arg4)
{
  return gdk_text_property_to_text_list(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gdk_text_property_to_utf8_list(struct _GdkAtom* arg0, int arg1, unsigned char const* arg2, int arg3, char*** arg4)
{
  return gdk_text_property_to_utf8_list(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gdk_utf8_to_compound_text(char const* arg0, struct _GdkAtom** arg1, int* arg2, unsigned char** arg3, int* arg4)
{
  return gdk_utf8_to_compound_text(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gdk_string_to_compound_text(char const* arg0, struct _GdkAtom** arg1, int* arg2, unsigned char** arg3, int* arg4)
{
  return gdk_string_to_compound_text(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gdk_text_property_to_text_list_for_display(GdkDisplay* arg0, struct _GdkAtom* arg1, int arg2, unsigned char const* arg3, int arg4, char*** arg5)
{
  return gdk_text_property_to_text_list_for_display(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gdk_text_property_to_utf8_list_for_display(GdkDisplay* arg0, struct _GdkAtom* arg1, int arg2, unsigned char const* arg3, int arg4, char*** arg5)
{
  return gdk_text_property_to_utf8_list_for_display(arg0, arg1, arg2, arg3, arg4, arg5);
}

char* Pure_gdk_utf8_to_string_target(char const* arg0)
{
  return gdk_utf8_to_string_target(arg0);
}

int Pure_gdk_string_to_compound_text_for_display(GdkDisplay* arg0, char const* arg1, struct _GdkAtom** arg2, int* arg3, unsigned char** arg4, int* arg5)
{
  return gdk_string_to_compound_text_for_display(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gdk_utf8_to_compound_text_for_display(GdkDisplay* arg0, char const* arg1, struct _GdkAtom** arg2, int* arg3, unsigned char** arg4, int* arg5)
{
  return gdk_utf8_to_compound_text_for_display(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gdk_free_text_list(char** arg0)
{
  return gdk_free_text_list(arg0);
}

void Pure_gdk_free_compound_text(unsigned char* arg0)
{
  return gdk_free_compound_text(arg0);
}

GdkRegion* Pure_gdk_region_new()
{
  return gdk_region_new();
}

GdkRegion* Pure_gdk_region_polygon(GdkPoint const* arg0, int arg1, unsigned int arg2)
{
  return gdk_region_polygon(arg0, arg1, arg2);
}

GdkRegion* Pure_gdk_region_copy(GdkRegion const* arg0)
{
  return gdk_region_copy(arg0);
}

GdkRegion* Pure_gdk_region_rectangle(GdkRectangle const* arg0)
{
  return gdk_region_rectangle(arg0);
}

void Pure_gdk_region_destroy(GdkRegion* arg0)
{
  return gdk_region_destroy(arg0);
}

void Pure_gdk_region_get_clipbox(GdkRegion const* arg0, GdkRectangle* arg1)
{
  return gdk_region_get_clipbox(arg0, arg1);
}

void Pure_gdk_region_get_rectangles(GdkRegion const* arg0, GdkRectangle** arg1, int* arg2)
{
  return gdk_region_get_rectangles(arg0, arg1, arg2);
}

int Pure_gdk_region_empty(GdkRegion const* arg0)
{
  return gdk_region_empty(arg0);
}

int Pure_gdk_region_equal(GdkRegion const* arg0, GdkRegion const* arg1)
{
  return gdk_region_equal(arg0, arg1);
}

int Pure_gdk_region_point_in(GdkRegion const* arg0, int arg1, int arg2)
{
  return gdk_region_point_in(arg0, arg1, arg2);
}

unsigned int Pure_gdk_region_rect_in(GdkRegion const* arg0, GdkRectangle const* arg1)
{
  return gdk_region_rect_in(arg0, arg1);
}

void Pure_gdk_region_offset(GdkRegion* arg0, int arg1, int arg2)
{
  return gdk_region_offset(arg0, arg1, arg2);
}

void Pure_gdk_region_shrink(GdkRegion* arg0, int arg1, int arg2)
{
  return gdk_region_shrink(arg0, arg1, arg2);
}

void Pure_gdk_region_union_with_rect(GdkRegion* arg0, GdkRectangle const* arg1)
{
  return gdk_region_union_with_rect(arg0, arg1);
}

void Pure_gdk_region_intersect(GdkRegion* arg0, GdkRegion const* arg1)
{
  return gdk_region_intersect(arg0, arg1);
}

void Pure_gdk_region_union(GdkRegion* arg0, GdkRegion const* arg1)
{
  return gdk_region_union(arg0, arg1);
}

void Pure_gdk_region_subtract(GdkRegion* arg0, GdkRegion const* arg1)
{
  return gdk_region_subtract(arg0, arg1);
}

void Pure_gdk_region_xor(GdkRegion* arg0, GdkRegion const* arg1)
{
  return gdk_region_xor(arg0, arg1);
}

void Pure_gdk_region_spans_intersect_foreach(GdkRegion* arg0, GdkSpan const* arg1, int arg2, int arg3, void* arg4, void* arg5)
{
  return gdk_region_spans_intersect_foreach(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gdk_selection_owner_set(GdkWindow* arg0, struct _GdkAtom* arg1, unsigned int arg2, int arg3)
{
  return gdk_selection_owner_set(arg0, arg1, arg2, arg3);
}

GdkWindow* Pure_gdk_selection_owner_get(struct _GdkAtom* arg0)
{
  return gdk_selection_owner_get(arg0);
}

int Pure_gdk_selection_owner_set_for_display(GdkDisplay* arg0, GdkWindow* arg1, struct _GdkAtom* arg2, unsigned int arg3, int arg4)
{
  return gdk_selection_owner_set_for_display(arg0, arg1, arg2, arg3, arg4);
}

GdkWindow* Pure_gdk_selection_owner_get_for_display(GdkDisplay* arg0, struct _GdkAtom* arg1)
{
  return gdk_selection_owner_get_for_display(arg0, arg1);
}

void Pure_gdk_selection_convert(GdkWindow* arg0, struct _GdkAtom* arg1, struct _GdkAtom* arg2, unsigned int arg3)
{
  return gdk_selection_convert(arg0, arg1, arg2, arg3);
}

int Pure_gdk_selection_property_get(GdkWindow* arg0, unsigned char** arg1, struct _GdkAtom** arg2, int* arg3)
{
  return gdk_selection_property_get(arg0, arg1, arg2, arg3);
}

void Pure_gdk_selection_send_notify(unsigned int arg0, struct _GdkAtom* arg1, struct _GdkAtom* arg2, struct _GdkAtom* arg3, unsigned int arg4)
{
  return gdk_selection_send_notify(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gdk_selection_send_notify_for_display(GdkDisplay* arg0, unsigned int arg1, struct _GdkAtom* arg2, struct _GdkAtom* arg3, struct _GdkAtom* arg4, unsigned int arg5)
{
  return gdk_selection_send_notify_for_display(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gdk_spawn_on_screen(GdkScreen* arg0, char const* arg1, char** arg2, char** arg3, unsigned int arg4, void* arg5, void* arg6, int* arg7, GError** arg8)
{
  return gdk_spawn_on_screen(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

int Pure_gdk_spawn_on_screen_with_pipes(GdkScreen* arg0, char const* arg1, char** arg2, char** arg3, unsigned int arg4, void* arg5, void* arg6, int* arg7, int* arg8, int* arg9, int* arg10, GError** arg11)
{
  return gdk_spawn_on_screen_with_pipes(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}

int Pure_gdk_spawn_command_line_on_screen(GdkScreen* arg0, char const* arg1, GError** arg2)
{
  return gdk_spawn_command_line_on_screen(arg0, arg1, arg2);
}

unsigned long Pure_gdk_window_object_get_type()
{
  return gdk_window_object_get_type();
}

GdkWindow* Pure_gdk_window_new(GdkWindow* arg0, GdkWindowAttr* arg1, int arg2)
{
  return gdk_window_new(arg0, arg1, arg2);
}

void Pure_gdk_window_destroy(GdkWindow* arg0)
{
  return gdk_window_destroy(arg0);
}

unsigned int Pure_gdk_window_get_window_type(GdkWindow* arg0)
{
  return gdk_window_get_window_type(arg0);
}

GdkWindow* Pure_gdk_window_at_pointer(int* arg0, int* arg1)
{
  return gdk_window_at_pointer(arg0, arg1);
}

void Pure_gdk_window_show(GdkWindow* arg0)
{
  return gdk_window_show(arg0);
}

void Pure_gdk_window_hide(GdkWindow* arg0)
{
  return gdk_window_hide(arg0);
}

void Pure_gdk_window_withdraw(GdkWindow* arg0)
{
  return gdk_window_withdraw(arg0);
}

void Pure_gdk_window_show_unraised(GdkWindow* arg0)
{
  return gdk_window_show_unraised(arg0);
}

void Pure_gdk_window_move(GdkWindow* arg0, int arg1, int arg2)
{
  return gdk_window_move(arg0, arg1, arg2);
}

void Pure_gdk_window_resize(GdkWindow* arg0, int arg1, int arg2)
{
  return gdk_window_resize(arg0, arg1, arg2);
}

void Pure_gdk_window_move_resize(GdkWindow* arg0, int arg1, int arg2, int arg3, int arg4)
{
  return gdk_window_move_resize(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gdk_window_reparent(GdkWindow* arg0, GdkWindow* arg1, int arg2, int arg3)
{
  return gdk_window_reparent(arg0, arg1, arg2, arg3);
}

void Pure_gdk_window_clear(GdkWindow* arg0)
{
  return gdk_window_clear(arg0);
}

void Pure_gdk_window_clear_area(GdkWindow* arg0, int arg1, int arg2, int arg3, int arg4)
{
  return gdk_window_clear_area(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gdk_window_clear_area_e(GdkWindow* arg0, int arg1, int arg2, int arg3, int arg4)
{
  return gdk_window_clear_area_e(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gdk_window_raise(GdkWindow* arg0)
{
  return gdk_window_raise(arg0);
}

void Pure_gdk_window_lower(GdkWindow* arg0)
{
  return gdk_window_lower(arg0);
}

void Pure_gdk_window_focus(GdkWindow* arg0, unsigned int arg1)
{
  return gdk_window_focus(arg0, arg1);
}

void Pure_gdk_window_set_user_data(GdkWindow* arg0, void* arg1)
{
  return gdk_window_set_user_data(arg0, arg1);
}

void Pure_gdk_window_set_override_redirect(GdkWindow* arg0, int arg1)
{
  return gdk_window_set_override_redirect(arg0, arg1);
}

void Pure_gdk_window_set_accept_focus(GdkWindow* arg0, int arg1)
{
  return gdk_window_set_accept_focus(arg0, arg1);
}

void Pure_gdk_window_set_focus_on_map(GdkWindow* arg0, int arg1)
{
  return gdk_window_set_focus_on_map(arg0, arg1);
}

void Pure_gdk_window_add_filter(GdkWindow* arg0, void* arg1, void* arg2)
{
  return gdk_window_add_filter(arg0, arg1, arg2);
}

void Pure_gdk_window_remove_filter(GdkWindow* arg0, void* arg1, void* arg2)
{
  return gdk_window_remove_filter(arg0, arg1, arg2);
}

void Pure_gdk_window_scroll(GdkWindow* arg0, int arg1, int arg2)
{
  return gdk_window_scroll(arg0, arg1, arg2);
}

void Pure_gdk_window_move_region(GdkWindow* arg0, GdkRegion const* arg1, int arg2, int arg3)
{
  return gdk_window_move_region(arg0, arg1, arg2, arg3);
}

void Pure_gdk_window_shape_combine_mask(GdkWindow* arg0, GdkBitmap* arg1, int arg2, int arg3)
{
  return gdk_window_shape_combine_mask(arg0, arg1, arg2, arg3);
}

void Pure_gdk_window_shape_combine_region(GdkWindow* arg0, GdkRegion const* arg1, int arg2, int arg3)
{
  return gdk_window_shape_combine_region(arg0, arg1, arg2, arg3);
}

void Pure_gdk_window_set_child_shapes(GdkWindow* arg0)
{
  return gdk_window_set_child_shapes(arg0);
}

void Pure_gdk_window_set_composited(GdkWindow* arg0, int arg1)
{
  return gdk_window_set_composited(arg0, arg1);
}

void Pure_gdk_window_merge_child_shapes(GdkWindow* arg0)
{
  return gdk_window_merge_child_shapes(arg0);
}

void Pure_gdk_window_input_shape_combine_mask(GdkWindow* arg0, GdkBitmap* arg1, int arg2, int arg3)
{
  return gdk_window_input_shape_combine_mask(arg0, arg1, arg2, arg3);
}

void Pure_gdk_window_input_shape_combine_region(GdkWindow* arg0, GdkRegion const* arg1, int arg2, int arg3)
{
  return gdk_window_input_shape_combine_region(arg0, arg1, arg2, arg3);
}

void Pure_gdk_window_set_child_input_shapes(GdkWindow* arg0)
{
  return gdk_window_set_child_input_shapes(arg0);
}

void Pure_gdk_window_merge_child_input_shapes(GdkWindow* arg0)
{
  return gdk_window_merge_child_input_shapes(arg0);
}

int Pure_gdk_window_is_visible(GdkWindow* arg0)
{
  return gdk_window_is_visible(arg0);
}

int Pure_gdk_window_is_viewable(GdkWindow* arg0)
{
  return gdk_window_is_viewable(arg0);
}

unsigned int Pure_gdk_window_get_state(GdkWindow* arg0)
{
  return gdk_window_get_state(arg0);
}

int Pure_gdk_window_set_static_gravities(GdkWindow* arg0, int arg1)
{
  return gdk_window_set_static_gravities(arg0, arg1);
}

GdkWindow* Pure_gdk_window_foreign_new(unsigned int arg0)
{
  return gdk_window_foreign_new(arg0);
}

GdkWindow* Pure_gdk_window_lookup(unsigned int arg0)
{
  return gdk_window_lookup(arg0);
}

GdkWindow* Pure_gdk_window_foreign_new_for_display(GdkDisplay* arg0, unsigned int arg1)
{
  return gdk_window_foreign_new_for_display(arg0, arg1);
}

GdkWindow* Pure_gdk_window_lookup_for_display(GdkDisplay* arg0, unsigned int arg1)
{
  return gdk_window_lookup_for_display(arg0, arg1);
}

void Pure_gdk_window_set_hints(GdkWindow* arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7)
{
  return gdk_window_set_hints(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gdk_window_set_type_hint(GdkWindow* arg0, unsigned int arg1)
{
  return gdk_window_set_type_hint(arg0, arg1);
}

unsigned int Pure_gdk_window_get_type_hint(GdkWindow* arg0)
{
  return gdk_window_get_type_hint(arg0);
}

void Pure_gdk_window_set_modal_hint(GdkWindow* arg0, int arg1)
{
  return gdk_window_set_modal_hint(arg0, arg1);
}

void Pure_gdk_window_set_skip_taskbar_hint(GdkWindow* arg0, int arg1)
{
  return gdk_window_set_skip_taskbar_hint(arg0, arg1);
}

void Pure_gdk_window_set_skip_pager_hint(GdkWindow* arg0, int arg1)
{
  return gdk_window_set_skip_pager_hint(arg0, arg1);
}

void Pure_gdk_window_set_urgency_hint(GdkWindow* arg0, int arg1)
{
  return gdk_window_set_urgency_hint(arg0, arg1);
}

void Pure_gdk_window_set_geometry_hints(GdkWindow* arg0, GdkGeometry const* arg1, unsigned int arg2)
{
  return gdk_window_set_geometry_hints(arg0, arg1, arg2);
}

void Pure_gdk_set_sm_client_id(char const* arg0)
{
  return gdk_set_sm_client_id(arg0);
}

void Pure_gdk_window_begin_paint_rect(GdkWindow* arg0, GdkRectangle const* arg1)
{
  return gdk_window_begin_paint_rect(arg0, arg1);
}

void Pure_gdk_window_begin_paint_region(GdkWindow* arg0, GdkRegion const* arg1)
{
  return gdk_window_begin_paint_region(arg0, arg1);
}

void Pure_gdk_window_end_paint(GdkWindow* arg0)
{
  return gdk_window_end_paint(arg0);
}

void Pure_gdk_window_set_title(GdkWindow* arg0, char const* arg1)
{
  return gdk_window_set_title(arg0, arg1);
}

void Pure_gdk_window_set_role(GdkWindow* arg0, char const* arg1)
{
  return gdk_window_set_role(arg0, arg1);
}

void Pure_gdk_window_set_startup_id(GdkWindow* arg0, char const* arg1)
{
  return gdk_window_set_startup_id(arg0, arg1);
}

void Pure_gdk_window_set_transient_for(GdkWindow* arg0, GdkWindow* arg1)
{
  return gdk_window_set_transient_for(arg0, arg1);
}

void Pure_gdk_window_set_background(GdkWindow* arg0, GdkColor const* arg1)
{
  return gdk_window_set_background(arg0, arg1);
}

void Pure_gdk_window_set_back_pixmap(GdkWindow* arg0, GdkPixmap* arg1, int arg2)
{
  return gdk_window_set_back_pixmap(arg0, arg1, arg2);
}

void Pure_gdk_window_set_cursor(GdkWindow* arg0, GdkCursor* arg1)
{
  return gdk_window_set_cursor(arg0, arg1);
}

void Pure_gdk_window_get_user_data(GdkWindow* arg0, void** arg1)
{
  return gdk_window_get_user_data(arg0, arg1);
}

void Pure_gdk_window_get_geometry(GdkWindow* arg0, int* arg1, int* arg2, int* arg3, int* arg4, int* arg5)
{
  return gdk_window_get_geometry(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gdk_window_get_position(GdkWindow* arg0, int* arg1, int* arg2)
{
  return gdk_window_get_position(arg0, arg1, arg2);
}

int Pure_gdk_window_get_origin(GdkWindow* arg0, int* arg1, int* arg2)
{
  return gdk_window_get_origin(arg0, arg1, arg2);
}

int Pure_gdk_window_get_deskrelative_origin(GdkWindow* arg0, int* arg1, int* arg2)
{
  return gdk_window_get_deskrelative_origin(arg0, arg1, arg2);
}

void Pure_gdk_window_get_root_origin(GdkWindow* arg0, int* arg1, int* arg2)
{
  return gdk_window_get_root_origin(arg0, arg1, arg2);
}

void Pure_gdk_window_get_frame_extents(GdkWindow* arg0, GdkRectangle* arg1)
{
  return gdk_window_get_frame_extents(arg0, arg1);
}

GdkWindow* Pure_gdk_window_get_pointer(GdkWindow* arg0, int* arg1, int* arg2, unsigned int* arg3)
{
  return gdk_window_get_pointer(arg0, arg1, arg2, arg3);
}

GdkWindow* Pure_gdk_window_get_parent(GdkWindow* arg0)
{
  return gdk_window_get_parent(arg0);
}

GdkWindow* Pure_gdk_window_get_toplevel(GdkWindow* arg0)
{
  return gdk_window_get_toplevel(arg0);
}

GList* Pure_gdk_window_get_children(GdkWindow* arg0)
{
  return gdk_window_get_children(arg0);
}

GList* Pure_gdk_window_peek_children(GdkWindow* arg0)
{
  return gdk_window_peek_children(arg0);
}

unsigned int Pure_gdk_window_get_events(GdkWindow* arg0)
{
  return gdk_window_get_events(arg0);
}

void Pure_gdk_window_set_events(GdkWindow* arg0, unsigned int arg1)
{
  return gdk_window_set_events(arg0, arg1);
}

void Pure_gdk_window_set_icon_list(GdkWindow* arg0, GList* arg1)
{
  return gdk_window_set_icon_list(arg0, arg1);
}

void Pure_gdk_window_set_icon(GdkWindow* arg0, GdkWindow* arg1, GdkPixmap* arg2, GdkBitmap* arg3)
{
  return gdk_window_set_icon(arg0, arg1, arg2, arg3);
}

void Pure_gdk_window_set_icon_name(GdkWindow* arg0, char const* arg1)
{
  return gdk_window_set_icon_name(arg0, arg1);
}

void Pure_gdk_window_set_group(GdkWindow* arg0, GdkWindow* arg1)
{
  return gdk_window_set_group(arg0, arg1);
}

GdkWindow* Pure_gdk_window_get_group(GdkWindow* arg0)
{
  return gdk_window_get_group(arg0);
}

void Pure_gdk_window_set_decorations(GdkWindow* arg0, unsigned int arg1)
{
  return gdk_window_set_decorations(arg0, arg1);
}

int Pure_gdk_window_get_decorations(GdkWindow* arg0, unsigned int* arg1)
{
  return gdk_window_get_decorations(arg0, arg1);
}

void Pure_gdk_window_set_functions(GdkWindow* arg0, unsigned int arg1)
{
  return gdk_window_set_functions(arg0, arg1);
}

GList* Pure_gdk_window_get_toplevels()
{
  return gdk_window_get_toplevels();
}

void Pure_gdk_window_beep(GdkWindow* arg0)
{
  return gdk_window_beep(arg0);
}

void Pure_gdk_window_iconify(GdkWindow* arg0)
{
  return gdk_window_iconify(arg0);
}

void Pure_gdk_window_deiconify(GdkWindow* arg0)
{
  return gdk_window_deiconify(arg0);
}

void Pure_gdk_window_stick(GdkWindow* arg0)
{
  return gdk_window_stick(arg0);
}

void Pure_gdk_window_unstick(GdkWindow* arg0)
{
  return gdk_window_unstick(arg0);
}

void Pure_gdk_window_maximize(GdkWindow* arg0)
{
  return gdk_window_maximize(arg0);
}

void Pure_gdk_window_unmaximize(GdkWindow* arg0)
{
  return gdk_window_unmaximize(arg0);
}

void Pure_gdk_window_fullscreen(GdkWindow* arg0)
{
  return gdk_window_fullscreen(arg0);
}

void Pure_gdk_window_unfullscreen(GdkWindow* arg0)
{
  return gdk_window_unfullscreen(arg0);
}

void Pure_gdk_window_set_keep_above(GdkWindow* arg0, int arg1)
{
  return gdk_window_set_keep_above(arg0, arg1);
}

void Pure_gdk_window_set_keep_below(GdkWindow* arg0, int arg1)
{
  return gdk_window_set_keep_below(arg0, arg1);
}

void Pure_gdk_window_set_opacity(GdkWindow* arg0, double arg1)
{
  return gdk_window_set_opacity(arg0, arg1);
}

void Pure_gdk_window_register_dnd(GdkWindow* arg0)
{
  return gdk_window_register_dnd(arg0);
}

void Pure_gdk_window_begin_resize_drag(GdkWindow* arg0, unsigned int arg1, int arg2, int arg3, int arg4, unsigned int arg5)
{
  return gdk_window_begin_resize_drag(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gdk_window_begin_move_drag(GdkWindow* arg0, int arg1, int arg2, int arg3, unsigned int arg4)
{
  return gdk_window_begin_move_drag(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gdk_window_invalidate_rect(GdkWindow* arg0, GdkRectangle const* arg1, int arg2)
{
  return gdk_window_invalidate_rect(arg0, arg1, arg2);
}

void Pure_gdk_window_invalidate_region(GdkWindow* arg0, GdkRegion const* arg1, int arg2)
{
  return gdk_window_invalidate_region(arg0, arg1, arg2);
}

void Pure_gdk_window_invalidate_maybe_recurse(GdkWindow* arg0, GdkRegion const* arg1, void* arg2, void* arg3)
{
  return gdk_window_invalidate_maybe_recurse(arg0, arg1, arg2, arg3);
}

GdkRegion* Pure_gdk_window_get_update_area(GdkWindow* arg0)
{
  return gdk_window_get_update_area(arg0);
}

void Pure_gdk_window_freeze_updates(GdkWindow* arg0)
{
  return gdk_window_freeze_updates(arg0);
}

void Pure_gdk_window_thaw_updates(GdkWindow* arg0)
{
  return gdk_window_thaw_updates(arg0);
}

void Pure_gdk_window_freeze_toplevel_updates_libgtk_only(GdkWindow* arg0)
{
  return gdk_window_freeze_toplevel_updates_libgtk_only(arg0);
}

void Pure_gdk_window_thaw_toplevel_updates_libgtk_only(GdkWindow* arg0)
{
  return gdk_window_thaw_toplevel_updates_libgtk_only(arg0);
}

void Pure_gdk_window_process_all_updates()
{
  return gdk_window_process_all_updates();
}

void Pure_gdk_window_process_updates(GdkWindow* arg0, int arg1)
{
  return gdk_window_process_updates(arg0, arg1);
}

void Pure_gdk_window_set_debug_updates(int arg0)
{
  return gdk_window_set_debug_updates(arg0);
}

void Pure_gdk_window_constrain_size(GdkGeometry* arg0, unsigned int arg1, int arg2, int arg3, int* arg4, int* arg5)
{
  return gdk_window_constrain_size(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gdk_window_get_internal_paint_info(GdkWindow* arg0, GdkDrawable** arg1, int* arg2, int* arg3)
{
  return gdk_window_get_internal_paint_info(arg0, arg1, arg2, arg3);
}

void Pure_gdk_window_enable_synchronized_configure(GdkWindow* arg0)
{
  return gdk_window_enable_synchronized_configure(arg0);
}

void Pure_gdk_window_configure_finished(GdkWindow* arg0)
{
  return gdk_window_configure_finished(arg0);
}

GdkPointerHooks* Pure_gdk_set_pointer_hooks(GdkPointerHooks const* arg0)
{
  return gdk_set_pointer_hooks(arg0);
}

GdkWindow* Pure_gdk_get_default_root_window()
{
  return gdk_get_default_root_window();
}

void Pure_gdk_window_redirect_to_drawable(GdkWindow* arg0, GdkDrawable* arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7)
{
  return gdk_window_redirect_to_drawable(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gdk_window_remove_redirection(GdkWindow* arg0)
{
  return gdk_window_remove_redirection(arg0);
}

void Pure_gdk_test_render_sync(GdkWindow* arg0)
{
  return gdk_test_render_sync(arg0);
}

int Pure_gdk_test_simulate_key(GdkWindow* arg0, int arg1, int arg2, unsigned int arg3, unsigned int arg4, int arg5)
{
  return gdk_test_simulate_key(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gdk_test_simulate_button(GdkWindow* arg0, int arg1, int arg2, unsigned int arg3, unsigned int arg4, int arg5)
{
  return gdk_test_simulate_button(arg0, arg1, arg2, arg3, arg4, arg5);
}

unsigned long Pure_gdk_visual_get_type()
{
  return gdk_visual_get_type();
}

int Pure_gdk_visual_get_best_depth()
{
  return gdk_visual_get_best_depth();
}

unsigned int Pure_gdk_visual_get_best_type()
{
  return gdk_visual_get_best_type();
}

GdkVisual* Pure_gdk_visual_get_system()
{
  return gdk_visual_get_system();
}

GdkVisual* Pure_gdk_visual_get_best()
{
  return gdk_visual_get_best();
}

GdkVisual* Pure_gdk_visual_get_best_with_depth(int arg0)
{
  return gdk_visual_get_best_with_depth(arg0);
}

GdkVisual* Pure_gdk_visual_get_best_with_type(unsigned int arg0)
{
  return gdk_visual_get_best_with_type(arg0);
}

GdkVisual* Pure_gdk_visual_get_best_with_both(int arg0, unsigned int arg1)
{
  return gdk_visual_get_best_with_both(arg0, arg1);
}

void Pure_gdk_query_depths(int** arg0, int* arg1)
{
  return gdk_query_depths(arg0, arg1);
}

void Pure_gdk_query_visual_types(unsigned int** arg0, int* arg1)
{
  return gdk_query_visual_types(arg0, arg1);
}

GList* Pure_gdk_list_visuals()
{
  return gdk_list_visuals();
}

GdkScreen* Pure_gdk_visual_get_screen(GdkVisual* arg0)
{
  return gdk_visual_get_screen(arg0);
}

void Pure_gdk_parse_args(int* arg0, char*** arg1)
{
  return gdk_parse_args(arg0, arg1);
}

void Pure_gdk_init(int* arg0, char*** arg1)
{
  return gdk_init(arg0, arg1);
}

int Pure_gdk_init_check(int* arg0, char*** arg1)
{
  return gdk_init_check(arg0, arg1);
}

void Pure_gdk_add_option_entries_libgtk_only(GOptionGroup* arg0)
{
  return gdk_add_option_entries_libgtk_only(arg0);
}

void Pure_gdk_pre_parse_libgtk_only()
{
  return gdk_pre_parse_libgtk_only();
}

void Pure_gdk_exit(int arg0)
{
  return gdk_exit(arg0);
}

char* Pure_gdk_set_locale()
{
  return gdk_set_locale();
}

char const* Pure_gdk_get_program_class()
{
  return gdk_get_program_class();
}

void Pure_gdk_set_program_class(char const* arg0)
{
  return gdk_set_program_class(arg0);
}

void Pure_gdk_error_trap_push()
{
  return gdk_error_trap_push();
}

int Pure_gdk_error_trap_pop()
{
  return gdk_error_trap_pop();
}

void Pure_gdk_set_use_xshm(int arg0)
{
  return gdk_set_use_xshm(arg0);
}

int Pure_gdk_get_use_xshm()
{
  return gdk_get_use_xshm();
}

char* Pure_gdk_get_display()
{
  return gdk_get_display();
}

char const* Pure_gdk_get_display_arg_name()
{
  return gdk_get_display_arg_name();
}

int Pure_gdk_input_add_full(int arg0, unsigned int arg1, void* arg2, void* arg3, void* arg4)
{
  return gdk_input_add_full(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gdk_input_add(int arg0, unsigned int arg1, void* arg2, void* arg3)
{
  return gdk_input_add(arg0, arg1, arg2, arg3);
}

void Pure_gdk_input_remove(int arg0)
{
  return gdk_input_remove(arg0);
}

unsigned int Pure_gdk_pointer_grab(GdkWindow* arg0, int arg1, unsigned int arg2, GdkWindow* arg3, GdkCursor* arg4, unsigned int arg5)
{
  return gdk_pointer_grab(arg0, arg1, arg2, arg3, arg4, arg5);
}

unsigned int Pure_gdk_keyboard_grab(GdkWindow* arg0, int arg1, unsigned int arg2)
{
  return gdk_keyboard_grab(arg0, arg1, arg2);
}

int Pure_gdk_pointer_grab_info_libgtk_only(GdkDisplay* arg0, GdkWindow** arg1, int* arg2)
{
  return gdk_pointer_grab_info_libgtk_only(arg0, arg1, arg2);
}

int Pure_gdk_keyboard_grab_info_libgtk_only(GdkDisplay* arg0, GdkWindow** arg1, int* arg2)
{
  return gdk_keyboard_grab_info_libgtk_only(arg0, arg1, arg2);
}

void Pure_gdk_pointer_ungrab(unsigned int arg0)
{
  return gdk_pointer_ungrab(arg0);
}

void Pure_gdk_keyboard_ungrab(unsigned int arg0)
{
  return gdk_keyboard_ungrab(arg0);
}

int Pure_gdk_pointer_is_grabbed()
{
  return gdk_pointer_is_grabbed();
}

int Pure_gdk_screen_width()
{
  return gdk_screen_width();
}

int Pure_gdk_screen_height()
{
  return gdk_screen_height();
}

int Pure_gdk_screen_width_mm()
{
  return gdk_screen_width_mm();
}

int Pure_gdk_screen_height_mm()
{
  return gdk_screen_height_mm();
}

void Pure_gdk_beep()
{
  return gdk_beep();
}

void Pure_gdk_flush()
{
  return gdk_flush();
}

void Pure_gdk_set_double_click_time(unsigned int arg0)
{
  return gdk_set_double_click_time(arg0);
}

int Pure_gdk_rectangle_intersect(GdkRectangle const* arg0, GdkRectangle const* arg1, GdkRectangle* arg2)
{
  return gdk_rectangle_intersect(arg0, arg1, arg2);
}

void Pure_gdk_rectangle_union(GdkRectangle const* arg0, GdkRectangle const* arg1, GdkRectangle* arg2)
{
  return gdk_rectangle_union(arg0, arg1, arg2);
}

unsigned long Pure_gdk_rectangle_get_type()
{
  return gdk_rectangle_get_type();
}

char* Pure_gdk_wcstombs(unsigned int const* arg0)
{
  return gdk_wcstombs(arg0);
}

int Pure_gdk_mbstowcs(unsigned int* arg0, char const* arg1, int arg2)
{
  return gdk_mbstowcs(arg0, arg1, arg2);
}

int Pure_gdk_event_send_client_message(GdkEvent* arg0, unsigned int arg1)
{
  return gdk_event_send_client_message(arg0, arg1);
}

void Pure_gdk_event_send_clientmessage_toall(GdkEvent* arg0)
{
  return gdk_event_send_clientmessage_toall(arg0);
}

int Pure_gdk_event_send_client_message_for_display(GdkDisplay* arg0, GdkEvent* arg1, unsigned int arg2)
{
  return gdk_event_send_client_message_for_display(arg0, arg1, arg2);
}

void Pure_gdk_notify_startup_complete()
{
  return gdk_notify_startup_complete();
}

void Pure_gdk_notify_startup_complete_with_id(char const* arg0)
{
  return gdk_notify_startup_complete_with_id(arg0);
}

void Pure_gdk_threads_enter()
{
  return gdk_threads_enter();
}

void Pure_gdk_threads_leave()
{
  return gdk_threads_leave();
}

void Pure_gdk_threads_init()
{
  return gdk_threads_init();
}

void Pure_gdk_threads_set_lock_functions(void* arg0, void* arg1)
{
  return gdk_threads_set_lock_functions(arg0, arg1);
}

unsigned int Pure_gdk_threads_add_idle_full(int arg0, void* arg1, void* arg2, void* arg3)
{
  return gdk_threads_add_idle_full(arg0, arg1, arg2, arg3);
}

unsigned int Pure_gdk_threads_add_idle(void* arg0, void* arg1)
{
  return gdk_threads_add_idle(arg0, arg1);
}

unsigned int Pure_gdk_threads_add_timeout_full(int arg0, unsigned int arg1, void* arg2, void* arg3, void* arg4)
{
  return gdk_threads_add_timeout_full(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_gdk_threads_add_timeout(unsigned int arg0, void* arg1, void* arg2)
{
  return gdk_threads_add_timeout(arg0, arg1, arg2);
}

unsigned int Pure_gdk_threads_add_timeout_seconds_full(int arg0, unsigned int arg1, void* arg2, void* arg3, void* arg4)
{
  return gdk_threads_add_timeout_seconds_full(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_gdk_threads_add_timeout_seconds(unsigned int arg0, void* arg1, void* arg2)
{
  return gdk_threads_add_timeout_seconds(arg0, arg1, arg2);
}
#include <gtk/gtk.h>

unsigned long Pure_gtk_accel_group_get_type()
{
  return gtk_accel_group_get_type();
}

GtkAccelGroup* Pure_gtk_accel_group_new()
{
  return gtk_accel_group_new();
}

int Pure_gtk_accel_group_get_is_locked(GtkAccelGroup* arg0)
{
  return gtk_accel_group_get_is_locked(arg0);
}

unsigned int Pure_gtk_accel_group_get_modifier_mask(GtkAccelGroup* arg0)
{
  return gtk_accel_group_get_modifier_mask(arg0);
}

void Pure_gtk_accel_group_lock(GtkAccelGroup* arg0)
{
  return gtk_accel_group_lock(arg0);
}

void Pure_gtk_accel_group_unlock(GtkAccelGroup* arg0)
{
  return gtk_accel_group_unlock(arg0);
}

void Pure_gtk_accel_group_connect(GtkAccelGroup* arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, GClosure* arg4)
{
  return gtk_accel_group_connect(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_accel_group_connect_by_path(GtkAccelGroup* arg0, char const* arg1, GClosure* arg2)
{
  return gtk_accel_group_connect_by_path(arg0, arg1, arg2);
}

int Pure_gtk_accel_group_disconnect(GtkAccelGroup* arg0, GClosure* arg1)
{
  return gtk_accel_group_disconnect(arg0, arg1);
}

int Pure_gtk_accel_group_disconnect_key(GtkAccelGroup* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_accel_group_disconnect_key(arg0, arg1, arg2);
}

int Pure_gtk_accel_group_activate(GtkAccelGroup* arg0, unsigned int arg1, GObject* arg2, unsigned int arg3, unsigned int arg4)
{
  return gtk_accel_group_activate(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gtk_accel_groups_activate(GObject* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_accel_groups_activate(arg0, arg1, arg2);
}

GSList* Pure_gtk_accel_groups_from_object(GObject* arg0)
{
  return gtk_accel_groups_from_object(arg0);
}

GtkAccelKey* Pure_gtk_accel_group_find(GtkAccelGroup* arg0, void* arg1, void* arg2)
{
  return gtk_accel_group_find(arg0, arg1, arg2);
}

GtkAccelGroup* Pure_gtk_accel_group_from_accel_closure(GClosure* arg0)
{
  return gtk_accel_group_from_accel_closure(arg0);
}

int Pure_gtk_accelerator_valid(unsigned int arg0, unsigned int arg1)
{
  return gtk_accelerator_valid(arg0, arg1);
}

void Pure_gtk_accelerator_parse(char const* arg0, unsigned int* arg1, unsigned int* arg2)
{
  return gtk_accelerator_parse(arg0, arg1, arg2);
}

char* Pure_gtk_accelerator_name(unsigned int arg0, unsigned int arg1)
{
  return gtk_accelerator_name(arg0, arg1);
}

char* Pure_gtk_accelerator_get_label(unsigned int arg0, unsigned int arg1)
{
  return gtk_accelerator_get_label(arg0, arg1);
}

void Pure_gtk_accelerator_set_default_mod_mask(unsigned int arg0)
{
  return gtk_accelerator_set_default_mod_mask(arg0);
}

unsigned int Pure_gtk_accelerator_get_default_mod_mask()
{
  return gtk_accelerator_get_default_mod_mask();
}

GtkAccelGroupEntry* Pure_gtk_accel_group_query(GtkAccelGroup* arg0, unsigned int arg1, unsigned int arg2, unsigned int* arg3)
{
  return gtk_accel_group_query(arg0, arg1, arg2, arg3);
}

unsigned long Pure_gtk_accel_flags_get_type()
{
  return gtk_accel_flags_get_type();
}

unsigned long Pure_gtk_assistant_page_type_get_type()
{
  return gtk_assistant_page_type_get_type();
}

unsigned long Pure_gtk_builder_error_get_type()
{
  return gtk_builder_error_get_type();
}

unsigned long Pure_gtk_calendar_display_options_get_type()
{
  return gtk_calendar_display_options_get_type();
}

unsigned long Pure_gtk_cell_renderer_state_get_type()
{
  return gtk_cell_renderer_state_get_type();
}

unsigned long Pure_gtk_cell_renderer_mode_get_type()
{
  return gtk_cell_renderer_mode_get_type();
}

unsigned long Pure_gtk_cell_renderer_accel_mode_get_type()
{
  return gtk_cell_renderer_accel_mode_get_type();
}

unsigned long Pure_gtk_debug_flag_get_type()
{
  return gtk_debug_flag_get_type();
}

unsigned long Pure_gtk_dialog_flags_get_type()
{
  return gtk_dialog_flags_get_type();
}

unsigned long Pure_gtk_response_type_get_type()
{
  return gtk_response_type_get_type();
}

unsigned long Pure_gtk_dest_defaults_get_type()
{
  return gtk_dest_defaults_get_type();
}

unsigned long Pure_gtk_target_flags_get_type()
{
  return gtk_target_flags_get_type();
}

unsigned long Pure_gtk_anchor_type_get_type()
{
  return gtk_anchor_type_get_type();
}

unsigned long Pure_gtk_arrow_type_get_type()
{
  return gtk_arrow_type_get_type();
}

unsigned long Pure_gtk_attach_options_get_type()
{
  return gtk_attach_options_get_type();
}

unsigned long Pure_gtk_button_box_style_get_type()
{
  return gtk_button_box_style_get_type();
}

unsigned long Pure_gtk_curve_type_get_type()
{
  return gtk_curve_type_get_type();
}

unsigned long Pure_gtk_delete_type_get_type()
{
  return gtk_delete_type_get_type();
}

unsigned long Pure_gtk_direction_type_get_type()
{
  return gtk_direction_type_get_type();
}

unsigned long Pure_gtk_expander_style_get_type()
{
  return gtk_expander_style_get_type();
}

unsigned long Pure_gtk_icon_size_get_type()
{
  return gtk_icon_size_get_type();
}

unsigned long Pure_gtk_sensitivity_type_get_type()
{
  return gtk_sensitivity_type_get_type();
}

unsigned long Pure_gtk_side_type_get_type()
{
  return gtk_side_type_get_type();
}

unsigned long Pure_gtk_text_direction_get_type()
{
  return gtk_text_direction_get_type();
}

unsigned long Pure_gtk_justification_get_type()
{
  return gtk_justification_get_type();
}

unsigned long Pure_gtk_match_type_get_type()
{
  return gtk_match_type_get_type();
}

unsigned long Pure_gtk_menu_direction_type_get_type()
{
  return gtk_menu_direction_type_get_type();
}

unsigned long Pure_gtk_metric_type_get_type()
{
  return gtk_metric_type_get_type();
}

unsigned long Pure_gtk_movement_step_get_type()
{
  return gtk_movement_step_get_type();
}

unsigned long Pure_gtk_scroll_step_get_type()
{
  return gtk_scroll_step_get_type();
}

unsigned long Pure_gtk_orientation_get_type()
{
  return gtk_orientation_get_type();
}

unsigned long Pure_gtk_corner_type_get_type()
{
  return gtk_corner_type_get_type();
}

unsigned long Pure_gtk_pack_type_get_type()
{
  return gtk_pack_type_get_type();
}

unsigned long Pure_gtk_path_priority_type_get_type()
{
  return gtk_path_priority_type_get_type();
}

unsigned long Pure_gtk_path_type_get_type()
{
  return gtk_path_type_get_type();
}

unsigned long Pure_gtk_policy_type_get_type()
{
  return gtk_policy_type_get_type();
}

unsigned long Pure_gtk_position_type_get_type()
{
  return gtk_position_type_get_type();
}

unsigned long Pure_gtk_preview_type_get_type()
{
  return gtk_preview_type_get_type();
}

unsigned long Pure_gtk_relief_style_get_type()
{
  return gtk_relief_style_get_type();
}

unsigned long Pure_gtk_resize_mode_get_type()
{
  return gtk_resize_mode_get_type();
}

unsigned long Pure_gtk_signal_run_type_get_type()
{
  return gtk_signal_run_type_get_type();
}

unsigned long Pure_gtk_scroll_type_get_type()
{
  return gtk_scroll_type_get_type();
}

unsigned long Pure_gtk_selection_mode_get_type()
{
  return gtk_selection_mode_get_type();
}

unsigned long Pure_gtk_shadow_type_get_type()
{
  return gtk_shadow_type_get_type();
}

unsigned long Pure_gtk_state_type_get_type()
{
  return gtk_state_type_get_type();
}

unsigned long Pure_gtk_submenu_direction_get_type()
{
  return gtk_submenu_direction_get_type();
}

unsigned long Pure_gtk_submenu_placement_get_type()
{
  return gtk_submenu_placement_get_type();
}

unsigned long Pure_gtk_toolbar_style_get_type()
{
  return gtk_toolbar_style_get_type();
}

unsigned long Pure_gtk_update_type_get_type()
{
  return gtk_update_type_get_type();
}

unsigned long Pure_gtk_visibility_get_type()
{
  return gtk_visibility_get_type();
}

unsigned long Pure_gtk_window_position_get_type()
{
  return gtk_window_position_get_type();
}

unsigned long Pure_gtk_window_type_get_type()
{
  return gtk_window_type_get_type();
}

unsigned long Pure_gtk_wrap_mode_get_type()
{
  return gtk_wrap_mode_get_type();
}

unsigned long Pure_gtk_sort_type_get_type()
{
  return gtk_sort_type_get_type();
}

unsigned long Pure_gtk_im_preedit_style_get_type()
{
  return gtk_im_preedit_style_get_type();
}

unsigned long Pure_gtk_im_status_style_get_type()
{
  return gtk_im_status_style_get_type();
}

unsigned long Pure_gtk_pack_direction_get_type()
{
  return gtk_pack_direction_get_type();
}

unsigned long Pure_gtk_print_pages_get_type()
{
  return gtk_print_pages_get_type();
}

unsigned long Pure_gtk_page_set_get_type()
{
  return gtk_page_set_get_type();
}

unsigned long Pure_gtk_number_up_layout_get_type()
{
  return gtk_number_up_layout_get_type();
}

unsigned long Pure_gtk_page_orientation_get_type()
{
  return gtk_page_orientation_get_type();
}

unsigned long Pure_gtk_print_quality_get_type()
{
  return gtk_print_quality_get_type();
}

unsigned long Pure_gtk_print_duplex_get_type()
{
  return gtk_print_duplex_get_type();
}

unsigned long Pure_gtk_unit_get_type()
{
  return gtk_unit_get_type();
}

unsigned long Pure_gtk_tree_view_grid_lines_get_type()
{
  return gtk_tree_view_grid_lines_get_type();
}

unsigned long Pure_gtk_drag_result_get_type()
{
  return gtk_drag_result_get_type();
}

unsigned long Pure_gtk_file_chooser_action_get_type()
{
  return gtk_file_chooser_action_get_type();
}

unsigned long Pure_gtk_file_chooser_confirmation_get_type()
{
  return gtk_file_chooser_confirmation_get_type();
}

unsigned long Pure_gtk_file_chooser_error_get_type()
{
  return gtk_file_chooser_error_get_type();
}

unsigned long Pure_gtk_file_filter_flags_get_type()
{
  return gtk_file_filter_flags_get_type();
}

unsigned long Pure_gtk_icon_lookup_flags_get_type()
{
  return gtk_icon_lookup_flags_get_type();
}

unsigned long Pure_gtk_icon_theme_error_get_type()
{
  return gtk_icon_theme_error_get_type();
}

unsigned long Pure_gtk_icon_view_drop_position_get_type()
{
  return gtk_icon_view_drop_position_get_type();
}

unsigned long Pure_gtk_image_type_get_type()
{
  return gtk_image_type_get_type();
}

unsigned long Pure_gtk_message_type_get_type()
{
  return gtk_message_type_get_type();
}

unsigned long Pure_gtk_buttons_type_get_type()
{
  return gtk_buttons_type_get_type();
}

unsigned long Pure_gtk_notebook_tab_get_type()
{
  return gtk_notebook_tab_get_type();
}

unsigned long Pure_gtk_object_flags_get_type()
{
  return gtk_object_flags_get_type();
}

unsigned long Pure_gtk_arg_flags_get_type()
{
  return gtk_arg_flags_get_type();
}

unsigned long Pure_gtk_print_status_get_type()
{
  return gtk_print_status_get_type();
}

unsigned long Pure_gtk_print_operation_result_get_type()
{
  return gtk_print_operation_result_get_type();
}

unsigned long Pure_gtk_print_operation_action_get_type()
{
  return gtk_print_operation_action_get_type();
}

unsigned long Pure_gtk_print_error_get_type()
{
  return gtk_print_error_get_type();
}

unsigned long Pure_gtk_private_flags_get_type()
{
  return gtk_private_flags_get_type();
}

unsigned long Pure_gtk_progress_bar_style_get_type()
{
  return gtk_progress_bar_style_get_type();
}

unsigned long Pure_gtk_progress_bar_orientation_get_type()
{
  return gtk_progress_bar_orientation_get_type();
}

unsigned long Pure_gtk_rc_flags_get_type()
{
  return gtk_rc_flags_get_type();
}

unsigned long Pure_gtk_rc_token_type_get_type()
{
  return gtk_rc_token_type_get_type();
}

unsigned long Pure_gtk_recent_sort_type_get_type()
{
  return gtk_recent_sort_type_get_type();
}

unsigned long Pure_gtk_recent_chooser_error_get_type()
{
  return gtk_recent_chooser_error_get_type();
}

unsigned long Pure_gtk_recent_filter_flags_get_type()
{
  return gtk_recent_filter_flags_get_type();
}

unsigned long Pure_gtk_recent_manager_error_get_type()
{
  return gtk_recent_manager_error_get_type();
}

unsigned long Pure_gtk_size_group_mode_get_type()
{
  return gtk_size_group_mode_get_type();
}

unsigned long Pure_gtk_spin_button_update_policy_get_type()
{
  return gtk_spin_button_update_policy_get_type();
}

unsigned long Pure_gtk_spin_type_get_type()
{
  return gtk_spin_type_get_type();
}

unsigned long Pure_gtk_text_buffer_target_info_get_type()
{
  return gtk_text_buffer_target_info_get_type();
}

unsigned long Pure_gtk_text_search_flags_get_type()
{
  return gtk_text_search_flags_get_type();
}

unsigned long Pure_gtk_text_window_type_get_type()
{
  return gtk_text_window_type_get_type();
}

unsigned long Pure_gtk_toolbar_child_type_get_type()
{
  return gtk_toolbar_child_type_get_type();
}

unsigned long Pure_gtk_toolbar_space_style_get_type()
{
  return gtk_toolbar_space_style_get_type();
}

unsigned long Pure_gtk_tree_model_flags_get_type()
{
  return gtk_tree_model_flags_get_type();
}

unsigned long Pure_gtk_tree_view_drop_position_get_type()
{
  return gtk_tree_view_drop_position_get_type();
}

unsigned long Pure_gtk_tree_view_column_sizing_get_type()
{
  return gtk_tree_view_column_sizing_get_type();
}

unsigned long Pure_gtk_ui_manager_item_type_get_type()
{
  return gtk_ui_manager_item_type_get_type();
}

unsigned long Pure_gtk_widget_flags_get_type()
{
  return gtk_widget_flags_get_type();
}

unsigned long Pure_gtk_widget_help_type_get_type()
{
  return gtk_widget_help_type_get_type();
}

unsigned long Pure_gtk_tree_view_mode_get_type()
{
  return gtk_tree_view_mode_get_type();
}

unsigned long Pure_gtk_cell_type_get_type()
{
  return gtk_cell_type_get_type();
}

unsigned long Pure_gtk_clist_drag_pos_get_type()
{
  return gtk_clist_drag_pos_get_type();
}

unsigned long Pure_gtk_button_action_get_type()
{
  return gtk_button_action_get_type();
}

unsigned long Pure_gtk_ctree_pos_get_type()
{
  return gtk_ctree_pos_get_type();
}

unsigned long Pure_gtk_ctree_line_style_get_type()
{
  return gtk_ctree_line_style_get_type();
}

unsigned long Pure_gtk_ctree_expander_style_get_type()
{
  return gtk_ctree_expander_style_get_type();
}

unsigned long Pure_gtk_ctree_expansion_type_get_type()
{
  return gtk_ctree_expansion_type_get_type();
}

unsigned long Pure_gtk_identifier_get_type()
{
  return gtk_identifier_get_type();
}

void Pure_gtk_type_init(unsigned int arg0)
{
  return gtk_type_init(arg0);
}

unsigned long Pure_gtk_type_unique(unsigned long arg0, GtkTypeInfo const* arg1)
{
  return gtk_type_unique(arg0, arg1);
}

void* Pure_gtk_type_class(unsigned long arg0)
{
  return gtk_type_class(arg0);
}

void* Pure_gtk_type_new(unsigned long arg0)
{
  return gtk_type_new(arg0);
}

GtkEnumValue* Pure_gtk_type_enum_get_values(unsigned long arg0)
{
  return gtk_type_enum_get_values(arg0);
}

GtkFlagValue* Pure_gtk_type_flags_get_values(unsigned long arg0)
{
  return gtk_type_flags_get_values(arg0);
}

GtkEnumValue* Pure_gtk_type_enum_find_value(unsigned long arg0, char const* arg1)
{
  return gtk_type_enum_find_value(arg0, arg1);
}

GtkFlagValue* Pure_gtk_type_flags_find_value(unsigned long arg0, char const* arg1)
{
  return gtk_type_flags_find_value(arg0, arg1);
}

unsigned long Pure_gtk_object_get_type()
{
  return gtk_object_get_type();
}

void Pure_gtk_object_sink(GtkObject* arg0)
{
  return gtk_object_sink(arg0);
}

void Pure_gtk_object_destroy(GtkObject* arg0)
{
  return gtk_object_destroy(arg0);
}

GtkObject* Pure_gtk_object_new(unsigned long arg0, char const* arg1)
{
  return gtk_object_new(arg0, arg1);
}

GtkObject* Pure_gtk_object_ref(GtkObject* arg0)
{
  return gtk_object_ref(arg0);
}

void Pure_gtk_object_unref(GtkObject* arg0)
{
  return gtk_object_unref(arg0);
}

void Pure_gtk_object_weakref(GtkObject* arg0, void* arg1, void* arg2)
{
  return gtk_object_weakref(arg0, arg1, arg2);
}

void Pure_gtk_object_weakunref(GtkObject* arg0, void* arg1, void* arg2)
{
  return gtk_object_weakunref(arg0, arg1, arg2);
}

void Pure_gtk_object_set_data(GtkObject* arg0, char const* arg1, void* arg2)
{
  return gtk_object_set_data(arg0, arg1, arg2);
}

void Pure_gtk_object_set_data_full(GtkObject* arg0, char const* arg1, void* arg2, void* arg3)
{
  return gtk_object_set_data_full(arg0, arg1, arg2, arg3);
}

void Pure_gtk_object_remove_data(GtkObject* arg0, char const* arg1)
{
  return gtk_object_remove_data(arg0, arg1);
}

void* Pure_gtk_object_get_data(GtkObject* arg0, char const* arg1)
{
  return gtk_object_get_data(arg0, arg1);
}

void Pure_gtk_object_remove_no_notify(GtkObject* arg0, char const* arg1)
{
  return gtk_object_remove_no_notify(arg0, arg1);
}

void Pure_gtk_object_set_user_data(GtkObject* arg0, void* arg1)
{
  return gtk_object_set_user_data(arg0, arg1);
}

void* Pure_gtk_object_get_user_data(GtkObject* arg0)
{
  return gtk_object_get_user_data(arg0);
}

void Pure_gtk_object_set_data_by_id(GtkObject* arg0, unsigned int arg1, void* arg2)
{
  return gtk_object_set_data_by_id(arg0, arg1, arg2);
}

void Pure_gtk_object_set_data_by_id_full(GtkObject* arg0, unsigned int arg1, void* arg2, void* arg3)
{
  return gtk_object_set_data_by_id_full(arg0, arg1, arg2, arg3);
}

void* Pure_gtk_object_get_data_by_id(GtkObject* arg0, unsigned int arg1)
{
  return gtk_object_get_data_by_id(arg0, arg1);
}

void Pure_gtk_object_remove_data_by_id(GtkObject* arg0, unsigned int arg1)
{
  return gtk_object_remove_data_by_id(arg0, arg1);
}

void Pure_gtk_object_remove_no_notify_by_id(GtkObject* arg0, unsigned int arg1)
{
  return gtk_object_remove_no_notify_by_id(arg0, arg1);
}

void Pure_gtk_object_get(GtkObject* arg0, char const* arg1)
{
  return gtk_object_get(arg0, arg1);
}

void Pure_gtk_object_set(GtkObject* arg0, char const* arg1)
{
  return gtk_object_set(arg0, arg1);
}

void Pure_gtk_object_add_arg_type(char const* arg0, unsigned long arg1, unsigned int arg2, unsigned int arg3)
{
  return gtk_object_add_arg_type(arg0, arg1, arg2, arg3);
}

unsigned long Pure_gtk_adjustment_get_type()
{
  return gtk_adjustment_get_type();
}

GtkObject* Pure_gtk_adjustment_new(double arg0, double arg1, double arg2, double arg3, double arg4, double arg5)
{
  return gtk_adjustment_new(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_adjustment_changed(GtkAdjustment* arg0)
{
  return gtk_adjustment_changed(arg0);
}

void Pure_gtk_adjustment_value_changed(GtkAdjustment* arg0)
{
  return gtk_adjustment_value_changed(arg0);
}

void Pure_gtk_adjustment_clamp_page(GtkAdjustment* arg0, double arg1, double arg2)
{
  return gtk_adjustment_clamp_page(arg0, arg1, arg2);
}

double Pure_gtk_adjustment_get_value(GtkAdjustment* arg0)
{
  return gtk_adjustment_get_value(arg0);
}

void Pure_gtk_adjustment_set_value(GtkAdjustment* arg0, double arg1)
{
  return gtk_adjustment_set_value(arg0, arg1);
}

double Pure_gtk_adjustment_get_lower(GtkAdjustment* arg0)
{
  return gtk_adjustment_get_lower(arg0);
}

void Pure_gtk_adjustment_set_lower(GtkAdjustment* arg0, double arg1)
{
  return gtk_adjustment_set_lower(arg0, arg1);
}

double Pure_gtk_adjustment_get_upper(GtkAdjustment* arg0)
{
  return gtk_adjustment_get_upper(arg0);
}

void Pure_gtk_adjustment_set_upper(GtkAdjustment* arg0, double arg1)
{
  return gtk_adjustment_set_upper(arg0, arg1);
}

double Pure_gtk_adjustment_get_step_increment(GtkAdjustment* arg0)
{
  return gtk_adjustment_get_step_increment(arg0);
}

void Pure_gtk_adjustment_set_step_increment(GtkAdjustment* arg0, double arg1)
{
  return gtk_adjustment_set_step_increment(arg0, arg1);
}

double Pure_gtk_adjustment_get_page_increment(GtkAdjustment* arg0)
{
  return gtk_adjustment_get_page_increment(arg0);
}

void Pure_gtk_adjustment_set_page_increment(GtkAdjustment* arg0, double arg1)
{
  return gtk_adjustment_set_page_increment(arg0, arg1);
}

double Pure_gtk_adjustment_get_page_size(GtkAdjustment* arg0)
{
  return gtk_adjustment_get_page_size(arg0);
}

void Pure_gtk_adjustment_set_page_size(GtkAdjustment* arg0, double arg1)
{
  return gtk_adjustment_set_page_size(arg0, arg1);
}

void Pure_gtk_adjustment_configure(GtkAdjustment* arg0, double arg1, double arg2, double arg3, double arg4, double arg5, double arg6)
{
  return gtk_adjustment_configure(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

unsigned long Pure_gtk_style_get_type()
{
  return gtk_style_get_type();
}

GtkStyle* Pure_gtk_style_new()
{
  return gtk_style_new();
}

GtkStyle* Pure_gtk_style_copy(GtkStyle* arg0)
{
  return gtk_style_copy(arg0);
}

GtkStyle* Pure_gtk_style_attach(GtkStyle* arg0, GdkWindow* arg1)
{
  return gtk_style_attach(arg0, arg1);
}

void Pure_gtk_style_detach(GtkStyle* arg0)
{
  return gtk_style_detach(arg0);
}

GtkStyle* Pure_gtk_style_ref(GtkStyle* arg0)
{
  return gtk_style_ref(arg0);
}

void Pure_gtk_style_unref(GtkStyle* arg0)
{
  return gtk_style_unref(arg0);
}

GdkFont* Pure_gtk_style_get_font(GtkStyle* arg0)
{
  return gtk_style_get_font(arg0);
}

void Pure_gtk_style_set_font(GtkStyle* arg0, GdkFont* arg1)
{
  return gtk_style_set_font(arg0, arg1);
}

void Pure_gtk_style_set_background(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2)
{
  return gtk_style_set_background(arg0, arg1, arg2);
}

void Pure_gtk_style_apply_default_background(GtkStyle* arg0, GdkWindow* arg1, int arg2, unsigned int arg3, GdkRectangle const* arg4, int arg5, int arg6, int arg7, int arg8)
{
  return gtk_style_apply_default_background(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

GtkIconSet* Pure_gtk_style_lookup_icon_set(GtkStyle* arg0, char const* arg1)
{
  return gtk_style_lookup_icon_set(arg0, arg1);
}

int Pure_gtk_style_lookup_color(GtkStyle* arg0, char const* arg1, GdkColor* arg2)
{
  return gtk_style_lookup_color(arg0, arg1, arg2);
}

GdkPixbuf* Pure_gtk_style_render_icon(GtkStyle* arg0, GtkIconSource const* arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, GtkWidget* arg5, char const* arg6)
{
  return gtk_style_render_icon(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_draw_hline(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, int arg3, int arg4, int arg5)
{
  return gtk_draw_hline(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_draw_vline(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, int arg3, int arg4, int arg5)
{
  return gtk_draw_vline(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_draw_shadow(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5, int arg6, int arg7)
{
  return gtk_draw_shadow(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gtk_draw_polygon(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkPoint* arg4, int arg5, int arg6)
{
  return gtk_draw_polygon(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_draw_arrow(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, int arg5, int arg6, int arg7, int arg8, int arg9)
{
  return gtk_draw_arrow(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_gtk_draw_diamond(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5, int arg6, int arg7)
{
  return gtk_draw_diamond(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gtk_draw_box(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5, int arg6, int arg7)
{
  return gtk_draw_box(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gtk_draw_flat_box(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5, int arg6, int arg7)
{
  return gtk_draw_flat_box(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gtk_draw_check(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5, int arg6, int arg7)
{
  return gtk_draw_check(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gtk_draw_option(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5, int arg6, int arg7)
{
  return gtk_draw_option(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gtk_draw_tab(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5, int arg6, int arg7)
{
  return gtk_draw_tab(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gtk_draw_shadow_gap(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, int arg9, int arg10)
{
  return gtk_draw_shadow_gap(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_gtk_draw_box_gap(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8, int arg9, int arg10)
{
  return gtk_draw_box_gap(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_gtk_draw_extension(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8)
{
  return gtk_draw_extension(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gtk_draw_focus(GtkStyle* arg0, GdkWindow* arg1, int arg2, int arg3, int arg4, int arg5)
{
  return gtk_draw_focus(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_draw_slider(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8)
{
  return gtk_draw_slider(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gtk_draw_handle(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5, int arg6, int arg7, unsigned int arg8)
{
  return gtk_draw_handle(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gtk_draw_expander(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, int arg3, int arg4, unsigned int arg5)
{
  return gtk_draw_expander(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_draw_layout(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, int arg3, int arg4, int arg5, PangoLayout* arg6)
{
  return gtk_draw_layout(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_draw_resize_grip(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, int arg4, int arg5, int arg6, int arg7)
{
  return gtk_draw_resize_grip(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gtk_paint_hline(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, GdkRectangle const* arg3, GtkWidget* arg4, char const* arg5, int arg6, int arg7, int arg8)
{
  return gtk_paint_hline(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gtk_paint_vline(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, GdkRectangle const* arg3, GtkWidget* arg4, char const* arg5, int arg6, int arg7, int arg8)
{
  return gtk_paint_vline(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gtk_paint_shadow(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, int arg7, int arg8, int arg9, int arg10)
{
  return gtk_paint_shadow(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_gtk_paint_polygon(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, GdkPoint const* arg7, int arg8, int arg9)
{
  return gtk_paint_polygon(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_gtk_paint_arrow(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, unsigned int arg7, int arg8, int arg9, int arg10, int arg11, int arg12)
{
  return gtk_paint_arrow(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}

void Pure_gtk_paint_diamond(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, int arg7, int arg8, int arg9, int arg10)
{
  return gtk_paint_diamond(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_gtk_paint_box(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, int arg7, int arg8, int arg9, int arg10)
{
  return gtk_paint_box(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_gtk_paint_flat_box(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, int arg7, int arg8, int arg9, int arg10)
{
  return gtk_paint_flat_box(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_gtk_paint_check(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, int arg7, int arg8, int arg9, int arg10)
{
  return gtk_paint_check(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_gtk_paint_option(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, int arg7, int arg8, int arg9, int arg10)
{
  return gtk_paint_option(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_gtk_paint_tab(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, int arg7, int arg8, int arg9, int arg10)
{
  return gtk_paint_tab(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_gtk_paint_shadow_gap(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, int arg7, int arg8, int arg9, int arg10, unsigned int arg11, int arg12, int arg13)
{
  return gtk_paint_shadow_gap(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
}

void Pure_gtk_paint_box_gap(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, int arg7, int arg8, int arg9, int arg10, unsigned int arg11, int arg12, int arg13)
{
  return gtk_paint_box_gap(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
}

void Pure_gtk_paint_extension(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, int arg7, int arg8, int arg9, int arg10, unsigned int arg11)
{
  return gtk_paint_extension(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}

void Pure_gtk_paint_focus(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, GdkRectangle const* arg3, GtkWidget* arg4, char const* arg5, int arg6, int arg7, int arg8, int arg9)
{
  return gtk_paint_focus(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_gtk_paint_slider(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, int arg7, int arg8, int arg9, int arg10, unsigned int arg11)
{
  return gtk_paint_slider(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}

void Pure_gtk_paint_handle(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, unsigned int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, int arg7, int arg8, int arg9, int arg10, unsigned int arg11)
{
  return gtk_paint_handle(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}

void Pure_gtk_paint_expander(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, GdkRectangle const* arg3, GtkWidget* arg4, char const* arg5, int arg6, int arg7, unsigned int arg8)
{
  return gtk_paint_expander(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gtk_paint_layout(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, int arg3, GdkRectangle const* arg4, GtkWidget* arg5, char const* arg6, int arg7, int arg8, PangoLayout* arg9)
{
  return gtk_paint_layout(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_gtk_paint_resize_grip(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, GdkRectangle const* arg3, GtkWidget* arg4, char const* arg5, unsigned int arg6, int arg7, int arg8, int arg9, int arg10)
{
  return gtk_paint_resize_grip(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

unsigned long Pure_gtk_border_get_type()
{
  return gtk_border_get_type();
}

GtkBorder* Pure_gtk_border_new()
{
  return gtk_border_new();
}

GtkBorder* Pure_gtk_border_copy(GtkBorder const* arg0)
{
  return gtk_border_copy(arg0);
}

void Pure_gtk_border_free(GtkBorder* arg0)
{
  return gtk_border_free(arg0);
}

void Pure_gtk_draw_string(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, int arg3, int arg4, char const* arg5)
{
  return gtk_draw_string(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_paint_string(GtkStyle* arg0, GdkWindow* arg1, unsigned int arg2, GdkRectangle const* arg3, GtkWidget* arg4, char const* arg5, int arg6, int arg7, char const* arg8)
{
  return gtk_paint_string(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gtk_draw_insertion_cursor(GtkWidget* arg0, GdkDrawable* arg1, GdkRectangle const* arg2, GdkRectangle const* arg3, int arg4, unsigned int arg5, int arg6)
{
  return gtk_draw_insertion_cursor(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_rc_add_default_file(char const* arg0)
{
  return gtk_rc_add_default_file(arg0);
}

void Pure_gtk_rc_set_default_files(char** arg0)
{
  return gtk_rc_set_default_files(arg0);
}

char** Pure_gtk_rc_get_default_files()
{
  return gtk_rc_get_default_files();
}

GtkStyle* Pure_gtk_rc_get_style(GtkWidget* arg0)
{
  return gtk_rc_get_style(arg0);
}

GtkStyle* Pure_gtk_rc_get_style_by_paths(GtkSettings* arg0, char const* arg1, char const* arg2, unsigned long arg3)
{
  return gtk_rc_get_style_by_paths(arg0, arg1, arg2, arg3);
}

int Pure_gtk_rc_reparse_all_for_settings(GtkSettings* arg0, int arg1)
{
  return gtk_rc_reparse_all_for_settings(arg0, arg1);
}

void Pure_gtk_rc_reset_styles(GtkSettings* arg0)
{
  return gtk_rc_reset_styles(arg0);
}

char* Pure_gtk_rc_find_pixmap_in_path(GtkSettings* arg0, GScanner* arg1, char const* arg2)
{
  return gtk_rc_find_pixmap_in_path(arg0, arg1, arg2);
}

void Pure_gtk_rc_parse(char const* arg0)
{
  return gtk_rc_parse(arg0);
}

void Pure_gtk_rc_parse_string(char const* arg0)
{
  return gtk_rc_parse_string(arg0);
}

int Pure_gtk_rc_reparse_all()
{
  return gtk_rc_reparse_all();
}

void Pure_gtk_rc_add_widget_name_style(GtkRcStyle* arg0, char const* arg1)
{
  return gtk_rc_add_widget_name_style(arg0, arg1);
}

void Pure_gtk_rc_add_widget_class_style(GtkRcStyle* arg0, char const* arg1)
{
  return gtk_rc_add_widget_class_style(arg0, arg1);
}

void Pure_gtk_rc_add_class_style(GtkRcStyle* arg0, char const* arg1)
{
  return gtk_rc_add_class_style(arg0, arg1);
}

unsigned long Pure_gtk_rc_style_get_type()
{
  return gtk_rc_style_get_type();
}

GtkRcStyle* Pure_gtk_rc_style_new()
{
  return gtk_rc_style_new();
}

GtkRcStyle* Pure_gtk_rc_style_copy(GtkRcStyle* arg0)
{
  return gtk_rc_style_copy(arg0);
}

void Pure_gtk_rc_style_ref(GtkRcStyle* arg0)
{
  return gtk_rc_style_ref(arg0);
}

void Pure_gtk_rc_style_unref(GtkRcStyle* arg0)
{
  return gtk_rc_style_unref(arg0);
}

char* Pure_gtk_rc_find_module_in_path(char const* arg0)
{
  return gtk_rc_find_module_in_path(arg0);
}

char* Pure_gtk_rc_get_theme_dir()
{
  return gtk_rc_get_theme_dir();
}

char* Pure_gtk_rc_get_module_dir()
{
  return gtk_rc_get_module_dir();
}

char* Pure_gtk_rc_get_im_module_path()
{
  return gtk_rc_get_im_module_path();
}

char* Pure_gtk_rc_get_im_module_file()
{
  return gtk_rc_get_im_module_file();
}

GScanner* Pure_gtk_rc_scanner_new()
{
  return gtk_rc_scanner_new();
}

unsigned int Pure_gtk_rc_parse_color(GScanner* arg0, GdkColor* arg1)
{
  return gtk_rc_parse_color(arg0, arg1);
}

unsigned int Pure_gtk_rc_parse_color_full(GScanner* arg0, GtkRcStyle* arg1, GdkColor* arg2)
{
  return gtk_rc_parse_color_full(arg0, arg1, arg2);
}

unsigned int Pure_gtk_rc_parse_state(GScanner* arg0, unsigned int* arg1)
{
  return gtk_rc_parse_state(arg0, arg1);
}

unsigned int Pure_gtk_rc_parse_priority(GScanner* arg0, unsigned int* arg1)
{
  return gtk_rc_parse_priority(arg0, arg1);
}

unsigned long Pure_gtk_settings_get_type()
{
  return gtk_settings_get_type();
}

GtkSettings* Pure_gtk_settings_get_default()
{
  return gtk_settings_get_default();
}

GtkSettings* Pure_gtk_settings_get_for_screen(GdkScreen* arg0)
{
  return gtk_settings_get_for_screen(arg0);
}

void Pure_gtk_settings_install_property(GParamSpec* arg0)
{
  return gtk_settings_install_property(arg0);
}

void Pure_gtk_settings_install_property_parser(GParamSpec* arg0, void* arg1)
{
  return gtk_settings_install_property_parser(arg0, arg1);
}

int Pure_gtk_rc_property_parse_color(GParamSpec const* arg0, GString const* arg1, GValue* arg2)
{
  return gtk_rc_property_parse_color(arg0, arg1, arg2);
}

int Pure_gtk_rc_property_parse_enum(GParamSpec const* arg0, GString const* arg1, GValue* arg2)
{
  return gtk_rc_property_parse_enum(arg0, arg1, arg2);
}

int Pure_gtk_rc_property_parse_flags(GParamSpec const* arg0, GString const* arg1, GValue* arg2)
{
  return gtk_rc_property_parse_flags(arg0, arg1, arg2);
}

int Pure_gtk_rc_property_parse_requisition(GParamSpec const* arg0, GString const* arg1, GValue* arg2)
{
  return gtk_rc_property_parse_requisition(arg0, arg1, arg2);
}

int Pure_gtk_rc_property_parse_border(GParamSpec const* arg0, GString const* arg1, GValue* arg2)
{
  return gtk_rc_property_parse_border(arg0, arg1, arg2);
}

void Pure_gtk_settings_set_property_value(GtkSettings* arg0, char const* arg1, GtkSettingsValue const* arg2)
{
  return gtk_settings_set_property_value(arg0, arg1, arg2);
}

void Pure_gtk_settings_set_string_property(GtkSettings* arg0, char const* arg1, char const* arg2, char const* arg3)
{
  return gtk_settings_set_string_property(arg0, arg1, arg2, arg3);
}

void Pure_gtk_settings_set_long_property(GtkSettings* arg0, char const* arg1, long arg2, char const* arg3)
{
  return gtk_settings_set_long_property(arg0, arg1, arg2, arg3);
}

void Pure_gtk_settings_set_double_property(GtkSettings* arg0, char const* arg1, double arg2, char const* arg3)
{
  return gtk_settings_set_double_property(arg0, arg1, arg2, arg3);
}

unsigned long Pure_gtk_widget_get_type()
{
  return gtk_widget_get_type();
}

GtkWidget* Pure_gtk_widget_new(unsigned long arg0, char const* arg1)
{
  return gtk_widget_new(arg0, arg1);
}

void Pure_gtk_widget_destroy(GtkWidget* arg0)
{
  return gtk_widget_destroy(arg0);
}

void Pure_gtk_widget_destroyed(GtkWidget* arg0, GtkWidget** arg1)
{
  return gtk_widget_destroyed(arg0, arg1);
}

GtkWidget* Pure_gtk_widget_ref(GtkWidget* arg0)
{
  return gtk_widget_ref(arg0);
}

void Pure_gtk_widget_unref(GtkWidget* arg0)
{
  return gtk_widget_unref(arg0);
}

void Pure_gtk_widget_set(GtkWidget* arg0, char const* arg1)
{
  return gtk_widget_set(arg0, arg1);
}

void Pure_gtk_widget_unparent(GtkWidget* arg0)
{
  return gtk_widget_unparent(arg0);
}

void Pure_gtk_widget_show(GtkWidget* arg0)
{
  return gtk_widget_show(arg0);
}

void Pure_gtk_widget_show_now(GtkWidget* arg0)
{
  return gtk_widget_show_now(arg0);
}

void Pure_gtk_widget_hide(GtkWidget* arg0)
{
  return gtk_widget_hide(arg0);
}

void Pure_gtk_widget_show_all(GtkWidget* arg0)
{
  return gtk_widget_show_all(arg0);
}

void Pure_gtk_widget_hide_all(GtkWidget* arg0)
{
  return gtk_widget_hide_all(arg0);
}

void Pure_gtk_widget_set_no_show_all(GtkWidget* arg0, int arg1)
{
  return gtk_widget_set_no_show_all(arg0, arg1);
}

int Pure_gtk_widget_get_no_show_all(GtkWidget* arg0)
{
  return gtk_widget_get_no_show_all(arg0);
}

void Pure_gtk_widget_map(GtkWidget* arg0)
{
  return gtk_widget_map(arg0);
}

void Pure_gtk_widget_unmap(GtkWidget* arg0)
{
  return gtk_widget_unmap(arg0);
}

void Pure_gtk_widget_realize(GtkWidget* arg0)
{
  return gtk_widget_realize(arg0);
}

void Pure_gtk_widget_unrealize(GtkWidget* arg0)
{
  return gtk_widget_unrealize(arg0);
}

void Pure_gtk_widget_queue_draw(GtkWidget* arg0)
{
  return gtk_widget_queue_draw(arg0);
}

void Pure_gtk_widget_queue_draw_area(GtkWidget* arg0, int arg1, int arg2, int arg3, int arg4)
{
  return gtk_widget_queue_draw_area(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_widget_queue_clear(GtkWidget* arg0)
{
  return gtk_widget_queue_clear(arg0);
}

void Pure_gtk_widget_queue_clear_area(GtkWidget* arg0, int arg1, int arg2, int arg3, int arg4)
{
  return gtk_widget_queue_clear_area(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_widget_queue_resize(GtkWidget* arg0)
{
  return gtk_widget_queue_resize(arg0);
}

void Pure_gtk_widget_queue_resize_no_redraw(GtkWidget* arg0)
{
  return gtk_widget_queue_resize_no_redraw(arg0);
}

void Pure_gtk_widget_draw(GtkWidget* arg0, GdkRectangle const* arg1)
{
  return gtk_widget_draw(arg0, arg1);
}

void Pure_gtk_widget_size_request(GtkWidget* arg0, GtkRequisition* arg1)
{
  return gtk_widget_size_request(arg0, arg1);
}

void Pure_gtk_widget_size_allocate(GtkWidget* arg0, GtkAllocation* arg1)
{
  return gtk_widget_size_allocate(arg0, arg1);
}

void Pure_gtk_widget_get_child_requisition(GtkWidget* arg0, GtkRequisition* arg1)
{
  return gtk_widget_get_child_requisition(arg0, arg1);
}

void Pure_gtk_widget_add_accelerator(GtkWidget* arg0, char const* arg1, GtkAccelGroup* arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5)
{
  return gtk_widget_add_accelerator(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gtk_widget_remove_accelerator(GtkWidget* arg0, GtkAccelGroup* arg1, unsigned int arg2, unsigned int arg3)
{
  return gtk_widget_remove_accelerator(arg0, arg1, arg2, arg3);
}

void Pure_gtk_widget_set_accel_path(GtkWidget* arg0, char const* arg1, GtkAccelGroup* arg2)
{
  return gtk_widget_set_accel_path(arg0, arg1, arg2);
}

GList* Pure_gtk_widget_list_accel_closures(GtkWidget* arg0)
{
  return gtk_widget_list_accel_closures(arg0);
}

int Pure_gtk_widget_can_activate_accel(GtkWidget* arg0, unsigned int arg1)
{
  return gtk_widget_can_activate_accel(arg0, arg1);
}

int Pure_gtk_widget_mnemonic_activate(GtkWidget* arg0, int arg1)
{
  return gtk_widget_mnemonic_activate(arg0, arg1);
}

int Pure_gtk_widget_event(GtkWidget* arg0, GdkEvent* arg1)
{
  return gtk_widget_event(arg0, arg1);
}

int Pure_gtk_widget_send_expose(GtkWidget* arg0, GdkEvent* arg1)
{
  return gtk_widget_send_expose(arg0, arg1);
}

int Pure_gtk_widget_activate(GtkWidget* arg0)
{
  return gtk_widget_activate(arg0);
}

int Pure_gtk_widget_set_scroll_adjustments(GtkWidget* arg0, GtkAdjustment* arg1, GtkAdjustment* arg2)
{
  return gtk_widget_set_scroll_adjustments(arg0, arg1, arg2);
}

void Pure_gtk_widget_reparent(GtkWidget* arg0, GtkWidget* arg1)
{
  return gtk_widget_reparent(arg0, arg1);
}

int Pure_gtk_widget_intersect(GtkWidget* arg0, GdkRectangle const* arg1, GdkRectangle* arg2)
{
  return gtk_widget_intersect(arg0, arg1, arg2);
}

GdkRegion* Pure_gtk_widget_region_intersect(GtkWidget* arg0, GdkRegion const* arg1)
{
  return gtk_widget_region_intersect(arg0, arg1);
}

void Pure_gtk_widget_freeze_child_notify(GtkWidget* arg0)
{
  return gtk_widget_freeze_child_notify(arg0);
}

void Pure_gtk_widget_child_notify(GtkWidget* arg0, char const* arg1)
{
  return gtk_widget_child_notify(arg0, arg1);
}

void Pure_gtk_widget_thaw_child_notify(GtkWidget* arg0)
{
  return gtk_widget_thaw_child_notify(arg0);
}

int Pure_gtk_widget_is_focus(GtkWidget* arg0)
{
  return gtk_widget_is_focus(arg0);
}

void Pure_gtk_widget_grab_focus(GtkWidget* arg0)
{
  return gtk_widget_grab_focus(arg0);
}

void Pure_gtk_widget_grab_default(GtkWidget* arg0)
{
  return gtk_widget_grab_default(arg0);
}

void Pure_gtk_widget_set_name(GtkWidget* arg0, char const* arg1)
{
  return gtk_widget_set_name(arg0, arg1);
}

char const* Pure_gtk_widget_get_name(GtkWidget* arg0)
{
  return gtk_widget_get_name(arg0);
}

void Pure_gtk_widget_set_state(GtkWidget* arg0, unsigned int arg1)
{
  return gtk_widget_set_state(arg0, arg1);
}

void Pure_gtk_widget_set_sensitive(GtkWidget* arg0, int arg1)
{
  return gtk_widget_set_sensitive(arg0, arg1);
}

void Pure_gtk_widget_set_app_paintable(GtkWidget* arg0, int arg1)
{
  return gtk_widget_set_app_paintable(arg0, arg1);
}

void Pure_gtk_widget_set_double_buffered(GtkWidget* arg0, int arg1)
{
  return gtk_widget_set_double_buffered(arg0, arg1);
}

void Pure_gtk_widget_set_redraw_on_allocate(GtkWidget* arg0, int arg1)
{
  return gtk_widget_set_redraw_on_allocate(arg0, arg1);
}

void Pure_gtk_widget_set_parent(GtkWidget* arg0, GtkWidget* arg1)
{
  return gtk_widget_set_parent(arg0, arg1);
}

GtkWidget* Pure_gtk_widget_get_parent(GtkWidget* arg0)
{
  return gtk_widget_get_parent(arg0);
}

void Pure_gtk_widget_set_parent_window(GtkWidget* arg0, GdkWindow* arg1)
{
  return gtk_widget_set_parent_window(arg0, arg1);
}

GdkWindow* Pure_gtk_widget_get_parent_window(GtkWidget* arg0)
{
  return gtk_widget_get_parent_window(arg0);
}

void Pure_gtk_widget_set_child_visible(GtkWidget* arg0, int arg1)
{
  return gtk_widget_set_child_visible(arg0, arg1);
}

int Pure_gtk_widget_get_child_visible(GtkWidget* arg0)
{
  return gtk_widget_get_child_visible(arg0);
}

GdkWindow* Pure_gtk_widget_get_window(GtkWidget* arg0)
{
  return gtk_widget_get_window(arg0);
}

int Pure_gtk_widget_child_focus(GtkWidget* arg0, unsigned int arg1)
{
  return gtk_widget_child_focus(arg0, arg1);
}

int Pure_gtk_widget_keynav_failed(GtkWidget* arg0, unsigned int arg1)
{
  return gtk_widget_keynav_failed(arg0, arg1);
}

void Pure_gtk_widget_error_bell(GtkWidget* arg0)
{
  return gtk_widget_error_bell(arg0);
}

void Pure_gtk_widget_set_size_request(GtkWidget* arg0, int arg1, int arg2)
{
  return gtk_widget_set_size_request(arg0, arg1, arg2);
}

void Pure_gtk_widget_get_size_request(GtkWidget* arg0, int* arg1, int* arg2)
{
  return gtk_widget_get_size_request(arg0, arg1, arg2);
}

void Pure_gtk_widget_set_uposition(GtkWidget* arg0, int arg1, int arg2)
{
  return gtk_widget_set_uposition(arg0, arg1, arg2);
}

void Pure_gtk_widget_set_usize(GtkWidget* arg0, int arg1, int arg2)
{
  return gtk_widget_set_usize(arg0, arg1, arg2);
}

void Pure_gtk_widget_set_events(GtkWidget* arg0, int arg1)
{
  return gtk_widget_set_events(arg0, arg1);
}

void Pure_gtk_widget_add_events(GtkWidget* arg0, int arg1)
{
  return gtk_widget_add_events(arg0, arg1);
}

void Pure_gtk_widget_set_extension_events(GtkWidget* arg0, unsigned int arg1)
{
  return gtk_widget_set_extension_events(arg0, arg1);
}

unsigned int Pure_gtk_widget_get_extension_events(GtkWidget* arg0)
{
  return gtk_widget_get_extension_events(arg0);
}

GtkWidget* Pure_gtk_widget_get_toplevel(GtkWidget* arg0)
{
  return gtk_widget_get_toplevel(arg0);
}

GtkWidget* Pure_gtk_widget_get_ancestor(GtkWidget* arg0, unsigned long arg1)
{
  return gtk_widget_get_ancestor(arg0, arg1);
}

GdkColormap* Pure_gtk_widget_get_colormap(GtkWidget* arg0)
{
  return gtk_widget_get_colormap(arg0);
}

GdkVisual* Pure_gtk_widget_get_visual(GtkWidget* arg0)
{
  return gtk_widget_get_visual(arg0);
}

GdkScreen* Pure_gtk_widget_get_screen(GtkWidget* arg0)
{
  return gtk_widget_get_screen(arg0);
}

int Pure_gtk_widget_has_screen(GtkWidget* arg0)
{
  return gtk_widget_has_screen(arg0);
}

GdkDisplay* Pure_gtk_widget_get_display(GtkWidget* arg0)
{
  return gtk_widget_get_display(arg0);
}

GdkWindow* Pure_gtk_widget_get_root_window(GtkWidget* arg0)
{
  return gtk_widget_get_root_window(arg0);
}

GtkSettings* Pure_gtk_widget_get_settings(GtkWidget* arg0)
{
  return gtk_widget_get_settings(arg0);
}

GtkClipboard* Pure_gtk_widget_get_clipboard(GtkWidget* arg0, struct _GdkAtom* arg1)
{
  return gtk_widget_get_clipboard(arg0, arg1);
}

GdkPixmap* Pure_gtk_widget_get_snapshot(GtkWidget* arg0, GdkRectangle* arg1)
{
  return gtk_widget_get_snapshot(arg0, arg1);
}

AtkObject* Pure_gtk_widget_get_accessible(GtkWidget* arg0)
{
  return gtk_widget_get_accessible(arg0);
}

void Pure_gtk_widget_set_colormap(GtkWidget* arg0, GdkColormap* arg1)
{
  return gtk_widget_set_colormap(arg0, arg1);
}

int Pure_gtk_widget_get_events(GtkWidget* arg0)
{
  return gtk_widget_get_events(arg0);
}

void Pure_gtk_widget_get_pointer(GtkWidget* arg0, int* arg1, int* arg2)
{
  return gtk_widget_get_pointer(arg0, arg1, arg2);
}

int Pure_gtk_widget_is_ancestor(GtkWidget* arg0, GtkWidget* arg1)
{
  return gtk_widget_is_ancestor(arg0, arg1);
}

int Pure_gtk_widget_translate_coordinates(GtkWidget* arg0, GtkWidget* arg1, int arg2, int arg3, int* arg4, int* arg5)
{
  return gtk_widget_translate_coordinates(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gtk_widget_hide_on_delete(GtkWidget* arg0)
{
  return gtk_widget_hide_on_delete(arg0);
}

void Pure_gtk_widget_set_style(GtkWidget* arg0, GtkStyle* arg1)
{
  return gtk_widget_set_style(arg0, arg1);
}

void Pure_gtk_widget_ensure_style(GtkWidget* arg0)
{
  return gtk_widget_ensure_style(arg0);
}

GtkStyle* Pure_gtk_widget_get_style(GtkWidget* arg0)
{
  return gtk_widget_get_style(arg0);
}

void Pure_gtk_widget_modify_style(GtkWidget* arg0, GtkRcStyle* arg1)
{
  return gtk_widget_modify_style(arg0, arg1);
}

GtkRcStyle* Pure_gtk_widget_get_modifier_style(GtkWidget* arg0)
{
  return gtk_widget_get_modifier_style(arg0);
}

void Pure_gtk_widget_modify_fg(GtkWidget* arg0, unsigned int arg1, GdkColor const* arg2)
{
  return gtk_widget_modify_fg(arg0, arg1, arg2);
}

void Pure_gtk_widget_modify_bg(GtkWidget* arg0, unsigned int arg1, GdkColor const* arg2)
{
  return gtk_widget_modify_bg(arg0, arg1, arg2);
}

void Pure_gtk_widget_modify_text(GtkWidget* arg0, unsigned int arg1, GdkColor const* arg2)
{
  return gtk_widget_modify_text(arg0, arg1, arg2);
}

void Pure_gtk_widget_modify_base(GtkWidget* arg0, unsigned int arg1, GdkColor const* arg2)
{
  return gtk_widget_modify_base(arg0, arg1, arg2);
}

void Pure_gtk_widget_modify_cursor(GtkWidget* arg0, GdkColor const* arg1, GdkColor const* arg2)
{
  return gtk_widget_modify_cursor(arg0, arg1, arg2);
}

void Pure_gtk_widget_modify_font(GtkWidget* arg0, PangoFontDescription* arg1)
{
  return gtk_widget_modify_font(arg0, arg1);
}

PangoContext* Pure_gtk_widget_create_pango_context(GtkWidget* arg0)
{
  return gtk_widget_create_pango_context(arg0);
}

PangoContext* Pure_gtk_widget_get_pango_context(GtkWidget* arg0)
{
  return gtk_widget_get_pango_context(arg0);
}

PangoLayout* Pure_gtk_widget_create_pango_layout(GtkWidget* arg0, char const* arg1)
{
  return gtk_widget_create_pango_layout(arg0, arg1);
}

GdkPixbuf* Pure_gtk_widget_render_icon(GtkWidget* arg0, char const* arg1, unsigned int arg2, char const* arg3)
{
  return gtk_widget_render_icon(arg0, arg1, arg2, arg3);
}

void Pure_gtk_widget_set_composite_name(GtkWidget* arg0, char const* arg1)
{
  return gtk_widget_set_composite_name(arg0, arg1);
}

char* Pure_gtk_widget_get_composite_name(GtkWidget* arg0)
{
  return gtk_widget_get_composite_name(arg0);
}

void Pure_gtk_widget_reset_rc_styles(GtkWidget* arg0)
{
  return gtk_widget_reset_rc_styles(arg0);
}

void Pure_gtk_widget_push_colormap(GdkColormap* arg0)
{
  return gtk_widget_push_colormap(arg0);
}

void Pure_gtk_widget_push_composite_child()
{
  return gtk_widget_push_composite_child();
}

void Pure_gtk_widget_pop_composite_child()
{
  return gtk_widget_pop_composite_child();
}

void Pure_gtk_widget_pop_colormap()
{
  return gtk_widget_pop_colormap();
}

void Pure_gtk_widget_class_install_style_property(GtkWidgetClass* arg0, GParamSpec* arg1)
{
  return gtk_widget_class_install_style_property(arg0, arg1);
}

void Pure_gtk_widget_class_install_style_property_parser(GtkWidgetClass* arg0, GParamSpec* arg1, void* arg2)
{
  return gtk_widget_class_install_style_property_parser(arg0, arg1, arg2);
}

GParamSpec* Pure_gtk_widget_class_find_style_property(GtkWidgetClass* arg0, char const* arg1)
{
  return gtk_widget_class_find_style_property(arg0, arg1);
}

GParamSpec** Pure_gtk_widget_class_list_style_properties(GtkWidgetClass* arg0, unsigned int* arg1)
{
  return gtk_widget_class_list_style_properties(arg0, arg1);
}

void Pure_gtk_widget_style_get_property(GtkWidget* arg0, char const* arg1, GValue* arg2)
{
  return gtk_widget_style_get_property(arg0, arg1, arg2);
}

void Pure_gtk_widget_style_get_valist(GtkWidget* arg0, char const* arg1, void* arg2)
{
  return gtk_widget_style_get_valist(arg0, arg1, arg2);
}

void Pure_gtk_widget_style_get(GtkWidget* arg0, char const* arg1)
{
  return gtk_widget_style_get(arg0, arg1);
}

void Pure_gtk_widget_set_default_colormap(GdkColormap* arg0)
{
  return gtk_widget_set_default_colormap(arg0);
}

GtkStyle* Pure_gtk_widget_get_default_style()
{
  return gtk_widget_get_default_style();
}

GdkColormap* Pure_gtk_widget_get_default_colormap()
{
  return gtk_widget_get_default_colormap();
}

GdkVisual* Pure_gtk_widget_get_default_visual()
{
  return gtk_widget_get_default_visual();
}

void Pure_gtk_widget_set_direction(GtkWidget* arg0, unsigned int arg1)
{
  return gtk_widget_set_direction(arg0, arg1);
}

unsigned int Pure_gtk_widget_get_direction(GtkWidget* arg0)
{
  return gtk_widget_get_direction(arg0);
}

void Pure_gtk_widget_set_default_direction(unsigned int arg0)
{
  return gtk_widget_set_default_direction(arg0);
}

unsigned int Pure_gtk_widget_get_default_direction()
{
  return gtk_widget_get_default_direction();
}

int Pure_gtk_widget_is_composited(GtkWidget* arg0)
{
  return gtk_widget_is_composited(arg0);
}

void Pure_gtk_widget_shape_combine_mask(GtkWidget* arg0, GdkBitmap* arg1, int arg2, int arg3)
{
  return gtk_widget_shape_combine_mask(arg0, arg1, arg2, arg3);
}

void Pure_gtk_widget_input_shape_combine_mask(GtkWidget* arg0, GdkBitmap* arg1, int arg2, int arg3)
{
  return gtk_widget_input_shape_combine_mask(arg0, arg1, arg2, arg3);
}

void Pure_gtk_widget_reset_shapes(GtkWidget* arg0)
{
  return gtk_widget_reset_shapes(arg0);
}

void Pure_gtk_widget_path(GtkWidget* arg0, unsigned int* arg1, char** arg2, char** arg3)
{
  return gtk_widget_path(arg0, arg1, arg2, arg3);
}

void Pure_gtk_widget_class_path(GtkWidget* arg0, unsigned int* arg1, char** arg2, char** arg3)
{
  return gtk_widget_class_path(arg0, arg1, arg2, arg3);
}

GList* Pure_gtk_widget_list_mnemonic_labels(GtkWidget* arg0)
{
  return gtk_widget_list_mnemonic_labels(arg0);
}

void Pure_gtk_widget_add_mnemonic_label(GtkWidget* arg0, GtkWidget* arg1)
{
  return gtk_widget_add_mnemonic_label(arg0, arg1);
}

void Pure_gtk_widget_remove_mnemonic_label(GtkWidget* arg0, GtkWidget* arg1)
{
  return gtk_widget_remove_mnemonic_label(arg0, arg1);
}

void Pure_gtk_widget_set_tooltip_window(GtkWidget* arg0, GtkWindow* arg1)
{
  return gtk_widget_set_tooltip_window(arg0, arg1);
}

GtkWindow* Pure_gtk_widget_get_tooltip_window(GtkWidget* arg0)
{
  return gtk_widget_get_tooltip_window(arg0);
}

void Pure_gtk_widget_trigger_tooltip_query(GtkWidget* arg0)
{
  return gtk_widget_trigger_tooltip_query(arg0);
}

void Pure_gtk_widget_set_tooltip_text(GtkWidget* arg0, char const* arg1)
{
  return gtk_widget_set_tooltip_text(arg0, arg1);
}

char* Pure_gtk_widget_get_tooltip_text(GtkWidget* arg0)
{
  return gtk_widget_get_tooltip_text(arg0);
}

void Pure_gtk_widget_set_tooltip_markup(GtkWidget* arg0, char const* arg1)
{
  return gtk_widget_set_tooltip_markup(arg0, arg1);
}

char* Pure_gtk_widget_get_tooltip_markup(GtkWidget* arg0)
{
  return gtk_widget_get_tooltip_markup(arg0);
}

void Pure_gtk_widget_set_has_tooltip(GtkWidget* arg0, int arg1)
{
  return gtk_widget_set_has_tooltip(arg0, arg1);
}

int Pure_gtk_widget_get_has_tooltip(GtkWidget* arg0)
{
  return gtk_widget_get_has_tooltip(arg0);
}

unsigned long Pure_gtk_requisition_get_type()
{
  return gtk_requisition_get_type();
}

GtkRequisition* Pure_gtk_requisition_copy(GtkRequisition const* arg0)
{
  return gtk_requisition_copy(arg0);
}

void Pure_gtk_requisition_free(GtkRequisition* arg0)
{
  return gtk_requisition_free(arg0);
}

unsigned long Pure_gtk_container_get_type()
{
  return gtk_container_get_type();
}

void Pure_gtk_container_set_border_width(GtkContainer* arg0, unsigned int arg1)
{
  return gtk_container_set_border_width(arg0, arg1);
}

unsigned int Pure_gtk_container_get_border_width(GtkContainer* arg0)
{
  return gtk_container_get_border_width(arg0);
}

void Pure_gtk_container_add(GtkContainer* arg0, GtkWidget* arg1)
{
  return gtk_container_add(arg0, arg1);
}

void Pure_gtk_container_remove(GtkContainer* arg0, GtkWidget* arg1)
{
  return gtk_container_remove(arg0, arg1);
}

void Pure_gtk_container_set_resize_mode(GtkContainer* arg0, unsigned int arg1)
{
  return gtk_container_set_resize_mode(arg0, arg1);
}

unsigned int Pure_gtk_container_get_resize_mode(GtkContainer* arg0)
{
  return gtk_container_get_resize_mode(arg0);
}

void Pure_gtk_container_check_resize(GtkContainer* arg0)
{
  return gtk_container_check_resize(arg0);
}

void Pure_gtk_container_foreach(GtkContainer* arg0, void* arg1, void* arg2)
{
  return gtk_container_foreach(arg0, arg1, arg2);
}

void Pure_gtk_container_foreach_full(GtkContainer* arg0, void* arg1, void* arg2, void* arg3, void* arg4)
{
  return gtk_container_foreach_full(arg0, arg1, arg2, arg3, arg4);
}

GList* Pure_gtk_container_get_children(GtkContainer* arg0)
{
  return gtk_container_get_children(arg0);
}

void Pure_gtk_container_propagate_expose(GtkContainer* arg0, GtkWidget* arg1, GdkEventExpose* arg2)
{
  return gtk_container_propagate_expose(arg0, arg1, arg2);
}

void Pure_gtk_container_set_focus_chain(GtkContainer* arg0, GList* arg1)
{
  return gtk_container_set_focus_chain(arg0, arg1);
}

int Pure_gtk_container_get_focus_chain(GtkContainer* arg0, GList** arg1)
{
  return gtk_container_get_focus_chain(arg0, arg1);
}

void Pure_gtk_container_unset_focus_chain(GtkContainer* arg0)
{
  return gtk_container_unset_focus_chain(arg0);
}

void Pure_gtk_container_set_reallocate_redraws(GtkContainer* arg0, int arg1)
{
  return gtk_container_set_reallocate_redraws(arg0, arg1);
}

void Pure_gtk_container_set_focus_child(GtkContainer* arg0, GtkWidget* arg1)
{
  return gtk_container_set_focus_child(arg0, arg1);
}

GtkWidget* Pure_gtk_container_get_focus_child(GtkContainer* arg0)
{
  return gtk_container_get_focus_child(arg0);
}

void Pure_gtk_container_set_focus_vadjustment(GtkContainer* arg0, GtkAdjustment* arg1)
{
  return gtk_container_set_focus_vadjustment(arg0, arg1);
}

GtkAdjustment* Pure_gtk_container_get_focus_vadjustment(GtkContainer* arg0)
{
  return gtk_container_get_focus_vadjustment(arg0);
}

void Pure_gtk_container_set_focus_hadjustment(GtkContainer* arg0, GtkAdjustment* arg1)
{
  return gtk_container_set_focus_hadjustment(arg0, arg1);
}

GtkAdjustment* Pure_gtk_container_get_focus_hadjustment(GtkContainer* arg0)
{
  return gtk_container_get_focus_hadjustment(arg0);
}

void Pure_gtk_container_resize_children(GtkContainer* arg0)
{
  return gtk_container_resize_children(arg0);
}

unsigned long Pure_gtk_container_child_type(GtkContainer* arg0)
{
  return gtk_container_child_type(arg0);
}

void Pure_gtk_container_class_install_child_property(GtkContainerClass* arg0, unsigned int arg1, GParamSpec* arg2)
{
  return gtk_container_class_install_child_property(arg0, arg1, arg2);
}

GParamSpec* Pure_gtk_container_class_find_child_property(GObjectClass* arg0, char const* arg1)
{
  return gtk_container_class_find_child_property(arg0, arg1);
}

GParamSpec** Pure_gtk_container_class_list_child_properties(GObjectClass* arg0, unsigned int* arg1)
{
  return gtk_container_class_list_child_properties(arg0, arg1);
}

void Pure_gtk_container_add_with_properties(GtkContainer* arg0, GtkWidget* arg1, char const* arg2)
{
  return gtk_container_add_with_properties(arg0, arg1, arg2);
}

void Pure_gtk_container_child_set(GtkContainer* arg0, GtkWidget* arg1, char const* arg2)
{
  return gtk_container_child_set(arg0, arg1, arg2);
}

void Pure_gtk_container_child_get(GtkContainer* arg0, GtkWidget* arg1, char const* arg2)
{
  return gtk_container_child_get(arg0, arg1, arg2);
}

void Pure_gtk_container_child_set_valist(GtkContainer* arg0, GtkWidget* arg1, char const* arg2, void* arg3)
{
  return gtk_container_child_set_valist(arg0, arg1, arg2, arg3);
}

void Pure_gtk_container_child_get_valist(GtkContainer* arg0, GtkWidget* arg1, char const* arg2, void* arg3)
{
  return gtk_container_child_get_valist(arg0, arg1, arg2, arg3);
}

void Pure_gtk_container_child_set_property(GtkContainer* arg0, GtkWidget* arg1, char const* arg2, GValue const* arg3)
{
  return gtk_container_child_set_property(arg0, arg1, arg2, arg3);
}

void Pure_gtk_container_child_get_property(GtkContainer* arg0, GtkWidget* arg1, char const* arg2, GValue* arg3)
{
  return gtk_container_child_get_property(arg0, arg1, arg2, arg3);
}

void Pure_gtk_container_forall(GtkContainer* arg0, void* arg1, void* arg2)
{
  return gtk_container_forall(arg0, arg1, arg2);
}

unsigned long Pure_gtk_bin_get_type()
{
  return gtk_bin_get_type();
}

GtkWidget* Pure_gtk_bin_get_child(GtkBin* arg0)
{
  return gtk_bin_get_child(arg0);
}

unsigned long Pure_gtk_window_get_type()
{
  return gtk_window_get_type();
}

GtkWidget* Pure_gtk_window_new(unsigned int arg0)
{
  return gtk_window_new(arg0);
}

void Pure_gtk_window_set_title(GtkWindow* arg0, char const* arg1)
{
  return gtk_window_set_title(arg0, arg1);
}

char const* Pure_gtk_window_get_title(GtkWindow* arg0)
{
  return gtk_window_get_title(arg0);
}

void Pure_gtk_window_set_wmclass(GtkWindow* arg0, char const* arg1, char const* arg2)
{
  return gtk_window_set_wmclass(arg0, arg1, arg2);
}

void Pure_gtk_window_set_role(GtkWindow* arg0, char const* arg1)
{
  return gtk_window_set_role(arg0, arg1);
}

void Pure_gtk_window_set_startup_id(GtkWindow* arg0, char const* arg1)
{
  return gtk_window_set_startup_id(arg0, arg1);
}

char const* Pure_gtk_window_get_role(GtkWindow* arg0)
{
  return gtk_window_get_role(arg0);
}

void Pure_gtk_window_add_accel_group(GtkWindow* arg0, GtkAccelGroup* arg1)
{
  return gtk_window_add_accel_group(arg0, arg1);
}

void Pure_gtk_window_remove_accel_group(GtkWindow* arg0, GtkAccelGroup* arg1)
{
  return gtk_window_remove_accel_group(arg0, arg1);
}

void Pure_gtk_window_set_position(GtkWindow* arg0, unsigned int arg1)
{
  return gtk_window_set_position(arg0, arg1);
}

int Pure_gtk_window_activate_focus(GtkWindow* arg0)
{
  return gtk_window_activate_focus(arg0);
}

void Pure_gtk_window_set_focus(GtkWindow* arg0, GtkWidget* arg1)
{
  return gtk_window_set_focus(arg0, arg1);
}

GtkWidget* Pure_gtk_window_get_focus(GtkWindow* arg0)
{
  return gtk_window_get_focus(arg0);
}

void Pure_gtk_window_set_default(GtkWindow* arg0, GtkWidget* arg1)
{
  return gtk_window_set_default(arg0, arg1);
}

GtkWidget* Pure_gtk_window_get_default_widget(GtkWindow* arg0)
{
  return gtk_window_get_default_widget(arg0);
}

int Pure_gtk_window_activate_default(GtkWindow* arg0)
{
  return gtk_window_activate_default(arg0);
}

void Pure_gtk_window_set_transient_for(GtkWindow* arg0, GtkWindow* arg1)
{
  return gtk_window_set_transient_for(arg0, arg1);
}

GtkWindow* Pure_gtk_window_get_transient_for(GtkWindow* arg0)
{
  return gtk_window_get_transient_for(arg0);
}

void Pure_gtk_window_set_opacity(GtkWindow* arg0, double arg1)
{
  return gtk_window_set_opacity(arg0, arg1);
}

double Pure_gtk_window_get_opacity(GtkWindow* arg0)
{
  return gtk_window_get_opacity(arg0);
}

void Pure_gtk_window_set_type_hint(GtkWindow* arg0, unsigned int arg1)
{
  return gtk_window_set_type_hint(arg0, arg1);
}

unsigned int Pure_gtk_window_get_type_hint(GtkWindow* arg0)
{
  return gtk_window_get_type_hint(arg0);
}

void Pure_gtk_window_set_skip_taskbar_hint(GtkWindow* arg0, int arg1)
{
  return gtk_window_set_skip_taskbar_hint(arg0, arg1);
}

int Pure_gtk_window_get_skip_taskbar_hint(GtkWindow* arg0)
{
  return gtk_window_get_skip_taskbar_hint(arg0);
}

void Pure_gtk_window_set_skip_pager_hint(GtkWindow* arg0, int arg1)
{
  return gtk_window_set_skip_pager_hint(arg0, arg1);
}

int Pure_gtk_window_get_skip_pager_hint(GtkWindow* arg0)
{
  return gtk_window_get_skip_pager_hint(arg0);
}

void Pure_gtk_window_set_urgency_hint(GtkWindow* arg0, int arg1)
{
  return gtk_window_set_urgency_hint(arg0, arg1);
}

int Pure_gtk_window_get_urgency_hint(GtkWindow* arg0)
{
  return gtk_window_get_urgency_hint(arg0);
}

void Pure_gtk_window_set_accept_focus(GtkWindow* arg0, int arg1)
{
  return gtk_window_set_accept_focus(arg0, arg1);
}

int Pure_gtk_window_get_accept_focus(GtkWindow* arg0)
{
  return gtk_window_get_accept_focus(arg0);
}

void Pure_gtk_window_set_focus_on_map(GtkWindow* arg0, int arg1)
{
  return gtk_window_set_focus_on_map(arg0, arg1);
}

int Pure_gtk_window_get_focus_on_map(GtkWindow* arg0)
{
  return gtk_window_get_focus_on_map(arg0);
}

void Pure_gtk_window_set_destroy_with_parent(GtkWindow* arg0, int arg1)
{
  return gtk_window_set_destroy_with_parent(arg0, arg1);
}

int Pure_gtk_window_get_destroy_with_parent(GtkWindow* arg0)
{
  return gtk_window_get_destroy_with_parent(arg0);
}

void Pure_gtk_window_set_resizable(GtkWindow* arg0, int arg1)
{
  return gtk_window_set_resizable(arg0, arg1);
}

int Pure_gtk_window_get_resizable(GtkWindow* arg0)
{
  return gtk_window_get_resizable(arg0);
}

void Pure_gtk_window_set_gravity(GtkWindow* arg0, unsigned int arg1)
{
  return gtk_window_set_gravity(arg0, arg1);
}

unsigned int Pure_gtk_window_get_gravity(GtkWindow* arg0)
{
  return gtk_window_get_gravity(arg0);
}

void Pure_gtk_window_set_geometry_hints(GtkWindow* arg0, GtkWidget* arg1, GdkGeometry* arg2, unsigned int arg3)
{
  return gtk_window_set_geometry_hints(arg0, arg1, arg2, arg3);
}

void Pure_gtk_window_set_screen(GtkWindow* arg0, GdkScreen* arg1)
{
  return gtk_window_set_screen(arg0, arg1);
}

GdkScreen* Pure_gtk_window_get_screen(GtkWindow* arg0)
{
  return gtk_window_get_screen(arg0);
}

int Pure_gtk_window_is_active(GtkWindow* arg0)
{
  return gtk_window_is_active(arg0);
}

int Pure_gtk_window_has_toplevel_focus(GtkWindow* arg0)
{
  return gtk_window_has_toplevel_focus(arg0);
}

void Pure_gtk_window_set_has_frame(GtkWindow* arg0, int arg1)
{
  return gtk_window_set_has_frame(arg0, arg1);
}

int Pure_gtk_window_get_has_frame(GtkWindow* arg0)
{
  return gtk_window_get_has_frame(arg0);
}

void Pure_gtk_window_set_frame_dimensions(GtkWindow* arg0, int arg1, int arg2, int arg3, int arg4)
{
  return gtk_window_set_frame_dimensions(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_window_get_frame_dimensions(GtkWindow* arg0, int* arg1, int* arg2, int* arg3, int* arg4)
{
  return gtk_window_get_frame_dimensions(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_window_set_decorated(GtkWindow* arg0, int arg1)
{
  return gtk_window_set_decorated(arg0, arg1);
}

int Pure_gtk_window_get_decorated(GtkWindow* arg0)
{
  return gtk_window_get_decorated(arg0);
}

void Pure_gtk_window_set_deletable(GtkWindow* arg0, int arg1)
{
  return gtk_window_set_deletable(arg0, arg1);
}

int Pure_gtk_window_get_deletable(GtkWindow* arg0)
{
  return gtk_window_get_deletable(arg0);
}

void Pure_gtk_window_set_icon_list(GtkWindow* arg0, GList* arg1)
{
  return gtk_window_set_icon_list(arg0, arg1);
}

GList* Pure_gtk_window_get_icon_list(GtkWindow* arg0)
{
  return gtk_window_get_icon_list(arg0);
}

void Pure_gtk_window_set_icon(GtkWindow* arg0, GdkPixbuf* arg1)
{
  return gtk_window_set_icon(arg0, arg1);
}

void Pure_gtk_window_set_icon_name(GtkWindow* arg0, char const* arg1)
{
  return gtk_window_set_icon_name(arg0, arg1);
}

int Pure_gtk_window_set_icon_from_file(GtkWindow* arg0, char const* arg1, GError** arg2)
{
  return gtk_window_set_icon_from_file(arg0, arg1, arg2);
}

GdkPixbuf* Pure_gtk_window_get_icon(GtkWindow* arg0)
{
  return gtk_window_get_icon(arg0);
}

char const* Pure_gtk_window_get_icon_name(GtkWindow* arg0)
{
  return gtk_window_get_icon_name(arg0);
}

void Pure_gtk_window_set_default_icon_list(GList* arg0)
{
  return gtk_window_set_default_icon_list(arg0);
}

GList* Pure_gtk_window_get_default_icon_list()
{
  return gtk_window_get_default_icon_list();
}

void Pure_gtk_window_set_default_icon(GdkPixbuf* arg0)
{
  return gtk_window_set_default_icon(arg0);
}

void Pure_gtk_window_set_default_icon_name(char const* arg0)
{
  return gtk_window_set_default_icon_name(arg0);
}

int Pure_gtk_window_set_default_icon_from_file(char const* arg0, GError** arg1)
{
  return gtk_window_set_default_icon_from_file(arg0, arg1);
}

void Pure_gtk_window_set_auto_startup_notification(int arg0)
{
  return gtk_window_set_auto_startup_notification(arg0);
}

void Pure_gtk_window_set_modal(GtkWindow* arg0, int arg1)
{
  return gtk_window_set_modal(arg0, arg1);
}

int Pure_gtk_window_get_modal(GtkWindow* arg0)
{
  return gtk_window_get_modal(arg0);
}

GList* Pure_gtk_window_list_toplevels()
{
  return gtk_window_list_toplevels();
}

void Pure_gtk_window_add_mnemonic(GtkWindow* arg0, unsigned int arg1, GtkWidget* arg2)
{
  return gtk_window_add_mnemonic(arg0, arg1, arg2);
}

void Pure_gtk_window_remove_mnemonic(GtkWindow* arg0, unsigned int arg1, GtkWidget* arg2)
{
  return gtk_window_remove_mnemonic(arg0, arg1, arg2);
}

int Pure_gtk_window_mnemonic_activate(GtkWindow* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_window_mnemonic_activate(arg0, arg1, arg2);
}

void Pure_gtk_window_set_mnemonic_modifier(GtkWindow* arg0, unsigned int arg1)
{
  return gtk_window_set_mnemonic_modifier(arg0, arg1);
}

unsigned int Pure_gtk_window_get_mnemonic_modifier(GtkWindow* arg0)
{
  return gtk_window_get_mnemonic_modifier(arg0);
}

int Pure_gtk_window_activate_key(GtkWindow* arg0, GdkEventKey* arg1)
{
  return gtk_window_activate_key(arg0, arg1);
}

int Pure_gtk_window_propagate_key_event(GtkWindow* arg0, GdkEventKey* arg1)
{
  return gtk_window_propagate_key_event(arg0, arg1);
}

void Pure_gtk_window_present(GtkWindow* arg0)
{
  return gtk_window_present(arg0);
}

void Pure_gtk_window_present_with_time(GtkWindow* arg0, unsigned int arg1)
{
  return gtk_window_present_with_time(arg0, arg1);
}

void Pure_gtk_window_iconify(GtkWindow* arg0)
{
  return gtk_window_iconify(arg0);
}

void Pure_gtk_window_deiconify(GtkWindow* arg0)
{
  return gtk_window_deiconify(arg0);
}

void Pure_gtk_window_stick(GtkWindow* arg0)
{
  return gtk_window_stick(arg0);
}

void Pure_gtk_window_unstick(GtkWindow* arg0)
{
  return gtk_window_unstick(arg0);
}

void Pure_gtk_window_maximize(GtkWindow* arg0)
{
  return gtk_window_maximize(arg0);
}

void Pure_gtk_window_unmaximize(GtkWindow* arg0)
{
  return gtk_window_unmaximize(arg0);
}

void Pure_gtk_window_fullscreen(GtkWindow* arg0)
{
  return gtk_window_fullscreen(arg0);
}

void Pure_gtk_window_unfullscreen(GtkWindow* arg0)
{
  return gtk_window_unfullscreen(arg0);
}

void Pure_gtk_window_set_keep_above(GtkWindow* arg0, int arg1)
{
  return gtk_window_set_keep_above(arg0, arg1);
}

void Pure_gtk_window_set_keep_below(GtkWindow* arg0, int arg1)
{
  return gtk_window_set_keep_below(arg0, arg1);
}

void Pure_gtk_window_begin_resize_drag(GtkWindow* arg0, unsigned int arg1, int arg2, int arg3, int arg4, unsigned int arg5)
{
  return gtk_window_begin_resize_drag(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_window_begin_move_drag(GtkWindow* arg0, int arg1, int arg2, int arg3, unsigned int arg4)
{
  return gtk_window_begin_move_drag(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_window_set_policy(GtkWindow* arg0, int arg1, int arg2, int arg3)
{
  return gtk_window_set_policy(arg0, arg1, arg2, arg3);
}

void Pure_gtk_window_set_default_size(GtkWindow* arg0, int arg1, int arg2)
{
  return gtk_window_set_default_size(arg0, arg1, arg2);
}

void Pure_gtk_window_get_default_size(GtkWindow* arg0, int* arg1, int* arg2)
{
  return gtk_window_get_default_size(arg0, arg1, arg2);
}

void Pure_gtk_window_resize(GtkWindow* arg0, int arg1, int arg2)
{
  return gtk_window_resize(arg0, arg1, arg2);
}

void Pure_gtk_window_get_size(GtkWindow* arg0, int* arg1, int* arg2)
{
  return gtk_window_get_size(arg0, arg1, arg2);
}

void Pure_gtk_window_move(GtkWindow* arg0, int arg1, int arg2)
{
  return gtk_window_move(arg0, arg1, arg2);
}

void Pure_gtk_window_get_position(GtkWindow* arg0, int* arg1, int* arg2)
{
  return gtk_window_get_position(arg0, arg1, arg2);
}

int Pure_gtk_window_parse_geometry(GtkWindow* arg0, char const* arg1)
{
  return gtk_window_parse_geometry(arg0, arg1);
}

GtkWindowGroup* Pure_gtk_window_get_group(GtkWindow* arg0)
{
  return gtk_window_get_group(arg0);
}

void Pure_gtk_window_reshow_with_initial_size(GtkWindow* arg0)
{
  return gtk_window_reshow_with_initial_size(arg0);
}

unsigned long Pure_gtk_window_group_get_type()
{
  return gtk_window_group_get_type();
}

GtkWindowGroup* Pure_gtk_window_group_new()
{
  return gtk_window_group_new();
}

void Pure_gtk_window_group_add_window(GtkWindowGroup* arg0, GtkWindow* arg1)
{
  return gtk_window_group_add_window(arg0, arg1);
}

void Pure_gtk_window_group_remove_window(GtkWindowGroup* arg0, GtkWindow* arg1)
{
  return gtk_window_group_remove_window(arg0, arg1);
}

GList* Pure_gtk_window_group_list_windows(GtkWindowGroup* arg0)
{
  return gtk_window_group_list_windows(arg0);
}

void Pure_gtk_window_remove_embedded_xid(GtkWindow* arg0, unsigned int arg1)
{
  return gtk_window_remove_embedded_xid(arg0, arg1);
}

void Pure_gtk_window_add_embedded_xid(GtkWindow* arg0, unsigned int arg1)
{
  return gtk_window_add_embedded_xid(arg0, arg1);
}

unsigned long Pure_gtk_dialog_get_type()
{
  return gtk_dialog_get_type();
}

GtkWidget* Pure_gtk_dialog_new()
{
  return gtk_dialog_new();
}

GtkWidget* Pure_gtk_dialog_new_with_buttons(char const* arg0, GtkWindow* arg1, unsigned int arg2, char const* arg3)
{
  return gtk_dialog_new_with_buttons(arg0, arg1, arg2, arg3);
}

void Pure_gtk_dialog_add_action_widget(GtkDialog* arg0, GtkWidget* arg1, int arg2)
{
  return gtk_dialog_add_action_widget(arg0, arg1, arg2);
}

GtkWidget* Pure_gtk_dialog_add_button(GtkDialog* arg0, char const* arg1, int arg2)
{
  return gtk_dialog_add_button(arg0, arg1, arg2);
}

void Pure_gtk_dialog_add_buttons(GtkDialog* arg0, char const* arg1)
{
  return gtk_dialog_add_buttons(arg0, arg1);
}

void Pure_gtk_dialog_set_response_sensitive(GtkDialog* arg0, int arg1, int arg2)
{
  return gtk_dialog_set_response_sensitive(arg0, arg1, arg2);
}

void Pure_gtk_dialog_set_default_response(GtkDialog* arg0, int arg1)
{
  return gtk_dialog_set_default_response(arg0, arg1);
}

int Pure_gtk_dialog_get_response_for_widget(GtkDialog* arg0, GtkWidget* arg1)
{
  return gtk_dialog_get_response_for_widget(arg0, arg1);
}

void Pure_gtk_dialog_set_has_separator(GtkDialog* arg0, int arg1)
{
  return gtk_dialog_set_has_separator(arg0, arg1);
}

int Pure_gtk_dialog_get_has_separator(GtkDialog* arg0)
{
  return gtk_dialog_get_has_separator(arg0);
}

int Pure_gtk_alternative_dialog_button_order(GdkScreen* arg0)
{
  return gtk_alternative_dialog_button_order(arg0);
}

void Pure_gtk_dialog_set_alternative_button_order(GtkDialog* arg0, int arg1)
{
  return gtk_dialog_set_alternative_button_order(arg0, arg1);
}

void Pure_gtk_dialog_set_alternative_button_order_from_array(GtkDialog* arg0, int arg1, int* arg2)
{
  return gtk_dialog_set_alternative_button_order_from_array(arg0, arg1, arg2);
}

void Pure_gtk_dialog_response(GtkDialog* arg0, int arg1)
{
  return gtk_dialog_response(arg0, arg1);
}

int Pure_gtk_dialog_run(GtkDialog* arg0)
{
  return gtk_dialog_run(arg0);
}

GtkWidget* Pure_gtk_dialog_get_action_area(GtkDialog* arg0)
{
  return gtk_dialog_get_action_area(arg0);
}

GtkWidget* Pure_gtk_dialog_get_content_area(GtkDialog* arg0)
{
  return gtk_dialog_get_content_area(arg0);
}

unsigned long Pure_gtk_about_dialog_get_type()
{
  return gtk_about_dialog_get_type();
}

GtkWidget* Pure_gtk_about_dialog_new()
{
  return gtk_about_dialog_new();
}

void Pure_gtk_show_about_dialog(GtkWindow* arg0, char const* arg1)
{
  return gtk_show_about_dialog(arg0, arg1);
}

char const* Pure_gtk_about_dialog_get_name(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_name(arg0);
}

void Pure_gtk_about_dialog_set_name(GtkAboutDialog* arg0, char const* arg1)
{
  return gtk_about_dialog_set_name(arg0, arg1);
}

char const* Pure_gtk_about_dialog_get_program_name(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_program_name(arg0);
}

void Pure_gtk_about_dialog_set_program_name(GtkAboutDialog* arg0, char const* arg1)
{
  return gtk_about_dialog_set_program_name(arg0, arg1);
}

char const* Pure_gtk_about_dialog_get_version(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_version(arg0);
}

void Pure_gtk_about_dialog_set_version(GtkAboutDialog* arg0, char const* arg1)
{
  return gtk_about_dialog_set_version(arg0, arg1);
}

char const* Pure_gtk_about_dialog_get_copyright(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_copyright(arg0);
}

void Pure_gtk_about_dialog_set_copyright(GtkAboutDialog* arg0, char const* arg1)
{
  return gtk_about_dialog_set_copyright(arg0, arg1);
}

char const* Pure_gtk_about_dialog_get_comments(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_comments(arg0);
}

void Pure_gtk_about_dialog_set_comments(GtkAboutDialog* arg0, char const* arg1)
{
  return gtk_about_dialog_set_comments(arg0, arg1);
}

char const* Pure_gtk_about_dialog_get_license(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_license(arg0);
}

void Pure_gtk_about_dialog_set_license(GtkAboutDialog* arg0, char const* arg1)
{
  return gtk_about_dialog_set_license(arg0, arg1);
}

int Pure_gtk_about_dialog_get_wrap_license(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_wrap_license(arg0);
}

void Pure_gtk_about_dialog_set_wrap_license(GtkAboutDialog* arg0, int arg1)
{
  return gtk_about_dialog_set_wrap_license(arg0, arg1);
}

char const* Pure_gtk_about_dialog_get_website(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_website(arg0);
}

void Pure_gtk_about_dialog_set_website(GtkAboutDialog* arg0, char const* arg1)
{
  return gtk_about_dialog_set_website(arg0, arg1);
}

char const* Pure_gtk_about_dialog_get_website_label(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_website_label(arg0);
}

void Pure_gtk_about_dialog_set_website_label(GtkAboutDialog* arg0, char const* arg1)
{
  return gtk_about_dialog_set_website_label(arg0, arg1);
}

char const* const* Pure_gtk_about_dialog_get_authors(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_authors(arg0);
}

void Pure_gtk_about_dialog_set_authors(GtkAboutDialog* arg0, char const** arg1)
{
  return gtk_about_dialog_set_authors(arg0, arg1);
}

char const* const* Pure_gtk_about_dialog_get_documenters(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_documenters(arg0);
}

void Pure_gtk_about_dialog_set_documenters(GtkAboutDialog* arg0, char const** arg1)
{
  return gtk_about_dialog_set_documenters(arg0, arg1);
}

char const* const* Pure_gtk_about_dialog_get_artists(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_artists(arg0);
}

void Pure_gtk_about_dialog_set_artists(GtkAboutDialog* arg0, char const** arg1)
{
  return gtk_about_dialog_set_artists(arg0, arg1);
}

char const* Pure_gtk_about_dialog_get_translator_credits(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_translator_credits(arg0);
}

void Pure_gtk_about_dialog_set_translator_credits(GtkAboutDialog* arg0, char const* arg1)
{
  return gtk_about_dialog_set_translator_credits(arg0, arg1);
}

GdkPixbuf* Pure_gtk_about_dialog_get_logo(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_logo(arg0);
}

void Pure_gtk_about_dialog_set_logo(GtkAboutDialog* arg0, GdkPixbuf* arg1)
{
  return gtk_about_dialog_set_logo(arg0, arg1);
}

char const* Pure_gtk_about_dialog_get_logo_icon_name(GtkAboutDialog* arg0)
{
  return gtk_about_dialog_get_logo_icon_name(arg0);
}

void Pure_gtk_about_dialog_set_logo_icon_name(GtkAboutDialog* arg0, char const* arg1)
{
  return gtk_about_dialog_set_logo_icon_name(arg0, arg1);
}

void* Pure_gtk_about_dialog_set_email_hook(void* arg0, void* arg1, void* arg2)
{
  return gtk_about_dialog_set_email_hook(arg0, arg1, arg2);
}

void* Pure_gtk_about_dialog_set_url_hook(void* arg0, void* arg1, void* arg2)
{
  return gtk_about_dialog_set_url_hook(arg0, arg1, arg2);
}

unsigned long Pure_gtk_misc_get_type()
{
  return gtk_misc_get_type();
}

void Pure_gtk_misc_set_alignment(GtkMisc* arg0, float arg1, float arg2)
{
  return gtk_misc_set_alignment(arg0, arg1, arg2);
}

void Pure_gtk_misc_get_alignment(GtkMisc* arg0, float* arg1, float* arg2)
{
  return gtk_misc_get_alignment(arg0, arg1, arg2);
}

void Pure_gtk_misc_set_padding(GtkMisc* arg0, int arg1, int arg2)
{
  return gtk_misc_set_padding(arg0, arg1, arg2);
}

void Pure_gtk_misc_get_padding(GtkMisc* arg0, int* arg1, int* arg2)
{
  return gtk_misc_get_padding(arg0, arg1, arg2);
}

unsigned long Pure_gtk_menu_shell_get_type()
{
  return gtk_menu_shell_get_type();
}

void Pure_gtk_menu_shell_append(GtkMenuShell* arg0, GtkWidget* arg1)
{
  return gtk_menu_shell_append(arg0, arg1);
}

void Pure_gtk_menu_shell_prepend(GtkMenuShell* arg0, GtkWidget* arg1)
{
  return gtk_menu_shell_prepend(arg0, arg1);
}

void Pure_gtk_menu_shell_insert(GtkMenuShell* arg0, GtkWidget* arg1, int arg2)
{
  return gtk_menu_shell_insert(arg0, arg1, arg2);
}

void Pure_gtk_menu_shell_deactivate(GtkMenuShell* arg0)
{
  return gtk_menu_shell_deactivate(arg0);
}

void Pure_gtk_menu_shell_select_item(GtkMenuShell* arg0, GtkWidget* arg1)
{
  return gtk_menu_shell_select_item(arg0, arg1);
}

void Pure_gtk_menu_shell_deselect(GtkMenuShell* arg0)
{
  return gtk_menu_shell_deselect(arg0);
}

void Pure_gtk_menu_shell_activate_item(GtkMenuShell* arg0, GtkWidget* arg1, int arg2)
{
  return gtk_menu_shell_activate_item(arg0, arg1, arg2);
}

void Pure_gtk_menu_shell_select_first(GtkMenuShell* arg0, int arg1)
{
  return gtk_menu_shell_select_first(arg0, arg1);
}

void Pure_gtk_menu_shell_cancel(GtkMenuShell* arg0)
{
  return gtk_menu_shell_cancel(arg0);
}

int Pure_gtk_menu_shell_get_take_focus(GtkMenuShell* arg0)
{
  return gtk_menu_shell_get_take_focus(arg0);
}

void Pure_gtk_menu_shell_set_take_focus(GtkMenuShell* arg0, int arg1)
{
  return gtk_menu_shell_set_take_focus(arg0, arg1);
}

unsigned long Pure_gtk_menu_get_type()
{
  return gtk_menu_get_type();
}

GtkWidget* Pure_gtk_menu_new()
{
  return gtk_menu_new();
}

void Pure_gtk_menu_popup(GtkMenu* arg0, GtkWidget* arg1, GtkWidget* arg2, void* arg3, void* arg4, unsigned int arg5, unsigned int arg6)
{
  return gtk_menu_popup(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_menu_reposition(GtkMenu* arg0)
{
  return gtk_menu_reposition(arg0);
}

void Pure_gtk_menu_popdown(GtkMenu* arg0)
{
  return gtk_menu_popdown(arg0);
}

GtkWidget* Pure_gtk_menu_get_active(GtkMenu* arg0)
{
  return gtk_menu_get_active(arg0);
}

void Pure_gtk_menu_set_active(GtkMenu* arg0, unsigned int arg1)
{
  return gtk_menu_set_active(arg0, arg1);
}

void Pure_gtk_menu_set_accel_group(GtkMenu* arg0, GtkAccelGroup* arg1)
{
  return gtk_menu_set_accel_group(arg0, arg1);
}

GtkAccelGroup* Pure_gtk_menu_get_accel_group(GtkMenu* arg0)
{
  return gtk_menu_get_accel_group(arg0);
}

void Pure_gtk_menu_set_accel_path(GtkMenu* arg0, char const* arg1)
{
  return gtk_menu_set_accel_path(arg0, arg1);
}

char const* Pure_gtk_menu_get_accel_path(GtkMenu* arg0)
{
  return gtk_menu_get_accel_path(arg0);
}

void Pure_gtk_menu_attach_to_widget(GtkMenu* arg0, GtkWidget* arg1, void* arg2)
{
  return gtk_menu_attach_to_widget(arg0, arg1, arg2);
}

void Pure_gtk_menu_detach(GtkMenu* arg0)
{
  return gtk_menu_detach(arg0);
}

GtkWidget* Pure_gtk_menu_get_attach_widget(GtkMenu* arg0)
{
  return gtk_menu_get_attach_widget(arg0);
}

void Pure_gtk_menu_set_tearoff_state(GtkMenu* arg0, int arg1)
{
  return gtk_menu_set_tearoff_state(arg0, arg1);
}

int Pure_gtk_menu_get_tearoff_state(GtkMenu* arg0)
{
  return gtk_menu_get_tearoff_state(arg0);
}

void Pure_gtk_menu_set_title(GtkMenu* arg0, char const* arg1)
{
  return gtk_menu_set_title(arg0, arg1);
}

char const* Pure_gtk_menu_get_title(GtkMenu* arg0)
{
  return gtk_menu_get_title(arg0);
}

void Pure_gtk_menu_reorder_child(GtkMenu* arg0, GtkWidget* arg1, int arg2)
{
  return gtk_menu_reorder_child(arg0, arg1, arg2);
}

void Pure_gtk_menu_set_screen(GtkMenu* arg0, GdkScreen* arg1)
{
  return gtk_menu_set_screen(arg0, arg1);
}

void Pure_gtk_menu_attach(GtkMenu* arg0, GtkWidget* arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5)
{
  return gtk_menu_attach(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_menu_set_monitor(GtkMenu* arg0, int arg1)
{
  return gtk_menu_set_monitor(arg0, arg1);
}

int Pure_gtk_menu_get_monitor(GtkMenu* arg0)
{
  return gtk_menu_get_monitor(arg0);
}

GList* Pure_gtk_menu_get_for_attach_widget(GtkWidget* arg0)
{
  return gtk_menu_get_for_attach_widget(arg0);
}

unsigned long Pure_gtk_label_get_type()
{
  return gtk_label_get_type();
}

GtkWidget* Pure_gtk_label_new(char const* arg0)
{
  return gtk_label_new(arg0);
}

GtkWidget* Pure_gtk_label_new_with_mnemonic(char const* arg0)
{
  return gtk_label_new_with_mnemonic(arg0);
}

void Pure_gtk_label_set_text(GtkLabel* arg0, char const* arg1)
{
  return gtk_label_set_text(arg0, arg1);
}

char const* Pure_gtk_label_get_text(GtkLabel* arg0)
{
  return gtk_label_get_text(arg0);
}

void Pure_gtk_label_set_attributes(GtkLabel* arg0, PangoAttrList* arg1)
{
  return gtk_label_set_attributes(arg0, arg1);
}

PangoAttrList* Pure_gtk_label_get_attributes(GtkLabel* arg0)
{
  return gtk_label_get_attributes(arg0);
}

void Pure_gtk_label_set_label(GtkLabel* arg0, char const* arg1)
{
  return gtk_label_set_label(arg0, arg1);
}

char const* Pure_gtk_label_get_label(GtkLabel* arg0)
{
  return gtk_label_get_label(arg0);
}

void Pure_gtk_label_set_markup(GtkLabel* arg0, char const* arg1)
{
  return gtk_label_set_markup(arg0, arg1);
}

void Pure_gtk_label_set_use_markup(GtkLabel* arg0, int arg1)
{
  return gtk_label_set_use_markup(arg0, arg1);
}

int Pure_gtk_label_get_use_markup(GtkLabel* arg0)
{
  return gtk_label_get_use_markup(arg0);
}

void Pure_gtk_label_set_use_underline(GtkLabel* arg0, int arg1)
{
  return gtk_label_set_use_underline(arg0, arg1);
}

int Pure_gtk_label_get_use_underline(GtkLabel* arg0)
{
  return gtk_label_get_use_underline(arg0);
}

void Pure_gtk_label_set_markup_with_mnemonic(GtkLabel* arg0, char const* arg1)
{
  return gtk_label_set_markup_with_mnemonic(arg0, arg1);
}

unsigned int Pure_gtk_label_get_mnemonic_keyval(GtkLabel* arg0)
{
  return gtk_label_get_mnemonic_keyval(arg0);
}

void Pure_gtk_label_set_mnemonic_widget(GtkLabel* arg0, GtkWidget* arg1)
{
  return gtk_label_set_mnemonic_widget(arg0, arg1);
}

GtkWidget* Pure_gtk_label_get_mnemonic_widget(GtkLabel* arg0)
{
  return gtk_label_get_mnemonic_widget(arg0);
}

void Pure_gtk_label_set_text_with_mnemonic(GtkLabel* arg0, char const* arg1)
{
  return gtk_label_set_text_with_mnemonic(arg0, arg1);
}

void Pure_gtk_label_set_justify(GtkLabel* arg0, unsigned int arg1)
{
  return gtk_label_set_justify(arg0, arg1);
}

unsigned int Pure_gtk_label_get_justify(GtkLabel* arg0)
{
  return gtk_label_get_justify(arg0);
}

void Pure_gtk_label_set_ellipsize(GtkLabel* arg0, unsigned int arg1)
{
  return gtk_label_set_ellipsize(arg0, arg1);
}

unsigned int Pure_gtk_label_get_ellipsize(GtkLabel* arg0)
{
  return gtk_label_get_ellipsize(arg0);
}

void Pure_gtk_label_set_width_chars(GtkLabel* arg0, int arg1)
{
  return gtk_label_set_width_chars(arg0, arg1);
}

int Pure_gtk_label_get_width_chars(GtkLabel* arg0)
{
  return gtk_label_get_width_chars(arg0);
}

void Pure_gtk_label_set_max_width_chars(GtkLabel* arg0, int arg1)
{
  return gtk_label_set_max_width_chars(arg0, arg1);
}

int Pure_gtk_label_get_max_width_chars(GtkLabel* arg0)
{
  return gtk_label_get_max_width_chars(arg0);
}

void Pure_gtk_label_set_pattern(GtkLabel* arg0, char const* arg1)
{
  return gtk_label_set_pattern(arg0, arg1);
}

void Pure_gtk_label_set_line_wrap(GtkLabel* arg0, int arg1)
{
  return gtk_label_set_line_wrap(arg0, arg1);
}

int Pure_gtk_label_get_line_wrap(GtkLabel* arg0)
{
  return gtk_label_get_line_wrap(arg0);
}

void Pure_gtk_label_set_line_wrap_mode(GtkLabel* arg0, unsigned int arg1)
{
  return gtk_label_set_line_wrap_mode(arg0, arg1);
}

unsigned int Pure_gtk_label_get_line_wrap_mode(GtkLabel* arg0)
{
  return gtk_label_get_line_wrap_mode(arg0);
}

void Pure_gtk_label_set_selectable(GtkLabel* arg0, int arg1)
{
  return gtk_label_set_selectable(arg0, arg1);
}

int Pure_gtk_label_get_selectable(GtkLabel* arg0)
{
  return gtk_label_get_selectable(arg0);
}

void Pure_gtk_label_set_angle(GtkLabel* arg0, double arg1)
{
  return gtk_label_set_angle(arg0, arg1);
}

double Pure_gtk_label_get_angle(GtkLabel* arg0)
{
  return gtk_label_get_angle(arg0);
}

void Pure_gtk_label_select_region(GtkLabel* arg0, int arg1, int arg2)
{
  return gtk_label_select_region(arg0, arg1, arg2);
}

int Pure_gtk_label_get_selection_bounds(GtkLabel* arg0, int* arg1, int* arg2)
{
  return gtk_label_get_selection_bounds(arg0, arg1, arg2);
}

PangoLayout* Pure_gtk_label_get_layout(GtkLabel* arg0)
{
  return gtk_label_get_layout(arg0);
}

void Pure_gtk_label_get_layout_offsets(GtkLabel* arg0, int* arg1, int* arg2)
{
  return gtk_label_get_layout_offsets(arg0, arg1, arg2);
}

void Pure_gtk_label_set_single_line_mode(GtkLabel* arg0, int arg1)
{
  return gtk_label_set_single_line_mode(arg0, arg1);
}

int Pure_gtk_label_get_single_line_mode(GtkLabel* arg0)
{
  return gtk_label_get_single_line_mode(arg0);
}

void Pure_gtk_label_get(GtkLabel* arg0, char** arg1)
{
  return gtk_label_get(arg0, arg1);
}

unsigned int Pure_gtk_label_parse_uline(GtkLabel* arg0, char const* arg1)
{
  return gtk_label_parse_uline(arg0, arg1);
}

unsigned long Pure_gtk_accel_label_get_type()
{
  return gtk_accel_label_get_type();
}

GtkWidget* Pure_gtk_accel_label_new(char const* arg0)
{
  return gtk_accel_label_new(arg0);
}

GtkWidget* Pure_gtk_accel_label_get_accel_widget(GtkAccelLabel* arg0)
{
  return gtk_accel_label_get_accel_widget(arg0);
}

unsigned int Pure_gtk_accel_label_get_accel_width(GtkAccelLabel* arg0)
{
  return gtk_accel_label_get_accel_width(arg0);
}

void Pure_gtk_accel_label_set_accel_widget(GtkAccelLabel* arg0, GtkWidget* arg1)
{
  return gtk_accel_label_set_accel_widget(arg0, arg1);
}

void Pure_gtk_accel_label_set_accel_closure(GtkAccelLabel* arg0, GClosure* arg1)
{
  return gtk_accel_label_set_accel_closure(arg0, arg1);
}

int Pure_gtk_accel_label_refetch(GtkAccelLabel* arg0)
{
  return gtk_accel_label_refetch(arg0);
}

void Pure_gtk_accel_map_add_entry(char const* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_accel_map_add_entry(arg0, arg1, arg2);
}

int Pure_gtk_accel_map_lookup_entry(char const* arg0, GtkAccelKey* arg1)
{
  return gtk_accel_map_lookup_entry(arg0, arg1);
}

int Pure_gtk_accel_map_change_entry(char const* arg0, unsigned int arg1, unsigned int arg2, int arg3)
{
  return gtk_accel_map_change_entry(arg0, arg1, arg2, arg3);
}

void Pure_gtk_accel_map_load(char const* arg0)
{
  return gtk_accel_map_load(arg0);
}

void Pure_gtk_accel_map_save(char const* arg0)
{
  return gtk_accel_map_save(arg0);
}

void Pure_gtk_accel_map_foreach(void* arg0, void* arg1)
{
  return gtk_accel_map_foreach(arg0, arg1);
}

void Pure_gtk_accel_map_load_fd(int arg0)
{
  return gtk_accel_map_load_fd(arg0);
}

void Pure_gtk_accel_map_load_scanner(GScanner* arg0)
{
  return gtk_accel_map_load_scanner(arg0);
}

void Pure_gtk_accel_map_save_fd(int arg0)
{
  return gtk_accel_map_save_fd(arg0);
}

void Pure_gtk_accel_map_lock_path(char const* arg0)
{
  return gtk_accel_map_lock_path(arg0);
}

void Pure_gtk_accel_map_unlock_path(char const* arg0)
{
  return gtk_accel_map_unlock_path(arg0);
}

void Pure_gtk_accel_map_add_filter(char const* arg0)
{
  return gtk_accel_map_add_filter(arg0);
}

void Pure_gtk_accel_map_foreach_unfiltered(void* arg0, void* arg1)
{
  return gtk_accel_map_foreach_unfiltered(arg0, arg1);
}

unsigned long Pure_gtk_accel_map_get_type()
{
  return gtk_accel_map_get_type();
}

GtkAccelMap* Pure_gtk_accel_map_get()
{
  return gtk_accel_map_get();
}

unsigned long Pure_gtk_accessible_get_type()
{
  return gtk_accessible_get_type();
}

void Pure_gtk_accessible_connect_widget_destroyed(GtkAccessible* arg0)
{
  return gtk_accessible_connect_widget_destroyed(arg0);
}

unsigned long Pure_gtk_action_get_type()
{
  return gtk_action_get_type();
}

GtkAction* Pure_gtk_action_new(char const* arg0, char const* arg1, char const* arg2, char const* arg3)
{
  return gtk_action_new(arg0, arg1, arg2, arg3);
}

char const* Pure_gtk_action_get_name(GtkAction* arg0)
{
  return gtk_action_get_name(arg0);
}

int Pure_gtk_action_is_sensitive(GtkAction* arg0)
{
  return gtk_action_is_sensitive(arg0);
}

int Pure_gtk_action_get_sensitive(GtkAction* arg0)
{
  return gtk_action_get_sensitive(arg0);
}

void Pure_gtk_action_set_sensitive(GtkAction* arg0, int arg1)
{
  return gtk_action_set_sensitive(arg0, arg1);
}

int Pure_gtk_action_is_visible(GtkAction* arg0)
{
  return gtk_action_is_visible(arg0);
}

int Pure_gtk_action_get_visible(GtkAction* arg0)
{
  return gtk_action_get_visible(arg0);
}

void Pure_gtk_action_set_visible(GtkAction* arg0, int arg1)
{
  return gtk_action_set_visible(arg0, arg1);
}

void Pure_gtk_action_activate(GtkAction* arg0)
{
  return gtk_action_activate(arg0);
}

GtkWidget* Pure_gtk_action_create_icon(GtkAction* arg0, unsigned int arg1)
{
  return gtk_action_create_icon(arg0, arg1);
}

GtkWidget* Pure_gtk_action_create_menu_item(GtkAction* arg0)
{
  return gtk_action_create_menu_item(arg0);
}

GtkWidget* Pure_gtk_action_create_tool_item(GtkAction* arg0)
{
  return gtk_action_create_tool_item(arg0);
}

GtkWidget* Pure_gtk_action_create_menu(GtkAction* arg0)
{
  return gtk_action_create_menu(arg0);
}

void Pure_gtk_action_connect_proxy(GtkAction* arg0, GtkWidget* arg1)
{
  return gtk_action_connect_proxy(arg0, arg1);
}

void Pure_gtk_action_disconnect_proxy(GtkAction* arg0, GtkWidget* arg1)
{
  return gtk_action_disconnect_proxy(arg0, arg1);
}

GSList* Pure_gtk_action_get_proxies(GtkAction* arg0)
{
  return gtk_action_get_proxies(arg0);
}

GtkAction* Pure_gtk_widget_get_action(GtkWidget* arg0)
{
  return gtk_widget_get_action(arg0);
}

void Pure_gtk_action_connect_accelerator(GtkAction* arg0)
{
  return gtk_action_connect_accelerator(arg0);
}

void Pure_gtk_action_disconnect_accelerator(GtkAction* arg0)
{
  return gtk_action_disconnect_accelerator(arg0);
}

char const* Pure_gtk_action_get_accel_path(GtkAction* arg0)
{
  return gtk_action_get_accel_path(arg0);
}

GClosure* Pure_gtk_action_get_accel_closure(GtkAction* arg0)
{
  return gtk_action_get_accel_closure(arg0);
}

void Pure_gtk_action_block_activate_from(GtkAction* arg0, GtkWidget* arg1)
{
  return gtk_action_block_activate_from(arg0, arg1);
}

void Pure_gtk_action_unblock_activate_from(GtkAction* arg0, GtkWidget* arg1)
{
  return gtk_action_unblock_activate_from(arg0, arg1);
}

void Pure_gtk_action_set_accel_path(GtkAction* arg0, char const* arg1)
{
  return gtk_action_set_accel_path(arg0, arg1);
}

void Pure_gtk_action_set_accel_group(GtkAction* arg0, GtkAccelGroup* arg1)
{
  return gtk_action_set_accel_group(arg0, arg1);
}

unsigned long Pure_gtk_action_group_get_type()
{
  return gtk_action_group_get_type();
}

GtkActionGroup* Pure_gtk_action_group_new(char const* arg0)
{
  return gtk_action_group_new(arg0);
}

char const* Pure_gtk_action_group_get_name(GtkActionGroup* arg0)
{
  return gtk_action_group_get_name(arg0);
}

int Pure_gtk_action_group_get_sensitive(GtkActionGroup* arg0)
{
  return gtk_action_group_get_sensitive(arg0);
}

void Pure_gtk_action_group_set_sensitive(GtkActionGroup* arg0, int arg1)
{
  return gtk_action_group_set_sensitive(arg0, arg1);
}

int Pure_gtk_action_group_get_visible(GtkActionGroup* arg0)
{
  return gtk_action_group_get_visible(arg0);
}

void Pure_gtk_action_group_set_visible(GtkActionGroup* arg0, int arg1)
{
  return gtk_action_group_set_visible(arg0, arg1);
}

GtkAction* Pure_gtk_action_group_get_action(GtkActionGroup* arg0, char const* arg1)
{
  return gtk_action_group_get_action(arg0, arg1);
}

GList* Pure_gtk_action_group_list_actions(GtkActionGroup* arg0)
{
  return gtk_action_group_list_actions(arg0);
}

void Pure_gtk_action_group_add_action(GtkActionGroup* arg0, GtkAction* arg1)
{
  return gtk_action_group_add_action(arg0, arg1);
}

void Pure_gtk_action_group_add_action_with_accel(GtkActionGroup* arg0, GtkAction* arg1, char const* arg2)
{
  return gtk_action_group_add_action_with_accel(arg0, arg1, arg2);
}

void Pure_gtk_action_group_remove_action(GtkActionGroup* arg0, GtkAction* arg1)
{
  return gtk_action_group_remove_action(arg0, arg1);
}

void Pure_gtk_action_group_add_actions(GtkActionGroup* arg0, GtkActionEntry const* arg1, unsigned int arg2, void* arg3)
{
  return gtk_action_group_add_actions(arg0, arg1, arg2, arg3);
}

void Pure_gtk_action_group_add_toggle_actions(GtkActionGroup* arg0, GtkToggleActionEntry const* arg1, unsigned int arg2, void* arg3)
{
  return gtk_action_group_add_toggle_actions(arg0, arg1, arg2, arg3);
}

void Pure_gtk_action_group_add_radio_actions(GtkActionGroup* arg0, GtkRadioActionEntry const* arg1, unsigned int arg2, int arg3, void* arg4, void* arg5)
{
  return gtk_action_group_add_radio_actions(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_action_group_add_actions_full(GtkActionGroup* arg0, GtkActionEntry const* arg1, unsigned int arg2, void* arg3, void* arg4)
{
  return gtk_action_group_add_actions_full(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_action_group_add_toggle_actions_full(GtkActionGroup* arg0, GtkToggleActionEntry const* arg1, unsigned int arg2, void* arg3, void* arg4)
{
  return gtk_action_group_add_toggle_actions_full(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_action_group_add_radio_actions_full(GtkActionGroup* arg0, GtkRadioActionEntry const* arg1, unsigned int arg2, int arg3, void* arg4, void* arg5, void* arg6)
{
  return gtk_action_group_add_radio_actions_full(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_action_group_set_translate_func(GtkActionGroup* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_action_group_set_translate_func(arg0, arg1, arg2, arg3);
}

void Pure_gtk_action_group_set_translation_domain(GtkActionGroup* arg0, char const* arg1)
{
  return gtk_action_group_set_translation_domain(arg0, arg1);
}

char const* Pure_gtk_action_group_translate_string(GtkActionGroup* arg0, char const* arg1)
{
  return gtk_action_group_translate_string(arg0, arg1);
}

unsigned long Pure_gtk_alignment_get_type()
{
  return gtk_alignment_get_type();
}

GtkWidget* Pure_gtk_alignment_new(float arg0, float arg1, float arg2, float arg3)
{
  return gtk_alignment_new(arg0, arg1, arg2, arg3);
}

void Pure_gtk_alignment_set(GtkAlignment* arg0, float arg1, float arg2, float arg3, float arg4)
{
  return gtk_alignment_set(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_alignment_set_padding(GtkAlignment* arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4)
{
  return gtk_alignment_set_padding(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_alignment_get_padding(GtkAlignment* arg0, unsigned int* arg1, unsigned int* arg2, unsigned int* arg3, unsigned int* arg4)
{
  return gtk_alignment_get_padding(arg0, arg1, arg2, arg3, arg4);
}

unsigned long Pure_gtk_arrow_get_type()
{
  return gtk_arrow_get_type();
}

GtkWidget* Pure_gtk_arrow_new(unsigned int arg0, unsigned int arg1)
{
  return gtk_arrow_new(arg0, arg1);
}

void Pure_gtk_arrow_set(GtkArrow* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_arrow_set(arg0, arg1, arg2);
}

unsigned long Pure_gtk_frame_get_type()
{
  return gtk_frame_get_type();
}

GtkWidget* Pure_gtk_frame_new(char const* arg0)
{
  return gtk_frame_new(arg0);
}

void Pure_gtk_frame_set_label(GtkFrame* arg0, char const* arg1)
{
  return gtk_frame_set_label(arg0, arg1);
}

char const* Pure_gtk_frame_get_label(GtkFrame* arg0)
{
  return gtk_frame_get_label(arg0);
}

void Pure_gtk_frame_set_label_widget(GtkFrame* arg0, GtkWidget* arg1)
{
  return gtk_frame_set_label_widget(arg0, arg1);
}

GtkWidget* Pure_gtk_frame_get_label_widget(GtkFrame* arg0)
{
  return gtk_frame_get_label_widget(arg0);
}

void Pure_gtk_frame_set_label_align(GtkFrame* arg0, float arg1, float arg2)
{
  return gtk_frame_set_label_align(arg0, arg1, arg2);
}

void Pure_gtk_frame_get_label_align(GtkFrame* arg0, float* arg1, float* arg2)
{
  return gtk_frame_get_label_align(arg0, arg1, arg2);
}

void Pure_gtk_frame_set_shadow_type(GtkFrame* arg0, unsigned int arg1)
{
  return gtk_frame_set_shadow_type(arg0, arg1);
}

unsigned int Pure_gtk_frame_get_shadow_type(GtkFrame* arg0)
{
  return gtk_frame_get_shadow_type(arg0);
}

unsigned long Pure_gtk_aspect_frame_get_type()
{
  return gtk_aspect_frame_get_type();
}

GtkWidget* Pure_gtk_aspect_frame_new(char const* arg0, float arg1, float arg2, float arg3, int arg4)
{
  return gtk_aspect_frame_new(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_aspect_frame_set(GtkAspectFrame* arg0, float arg1, float arg2, float arg3, int arg4)
{
  return gtk_aspect_frame_set(arg0, arg1, arg2, arg3, arg4);
}

unsigned long Pure_gtk_assistant_get_type()
{
  return gtk_assistant_get_type();
}

GtkWidget* Pure_gtk_assistant_new()
{
  return gtk_assistant_new();
}

int Pure_gtk_assistant_get_current_page(GtkAssistant* arg0)
{
  return gtk_assistant_get_current_page(arg0);
}

void Pure_gtk_assistant_set_current_page(GtkAssistant* arg0, int arg1)
{
  return gtk_assistant_set_current_page(arg0, arg1);
}

int Pure_gtk_assistant_get_n_pages(GtkAssistant* arg0)
{
  return gtk_assistant_get_n_pages(arg0);
}

GtkWidget* Pure_gtk_assistant_get_nth_page(GtkAssistant* arg0, int arg1)
{
  return gtk_assistant_get_nth_page(arg0, arg1);
}

int Pure_gtk_assistant_prepend_page(GtkAssistant* arg0, GtkWidget* arg1)
{
  return gtk_assistant_prepend_page(arg0, arg1);
}

int Pure_gtk_assistant_append_page(GtkAssistant* arg0, GtkWidget* arg1)
{
  return gtk_assistant_append_page(arg0, arg1);
}

int Pure_gtk_assistant_insert_page(GtkAssistant* arg0, GtkWidget* arg1, int arg2)
{
  return gtk_assistant_insert_page(arg0, arg1, arg2);
}

void Pure_gtk_assistant_set_forward_page_func(GtkAssistant* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_assistant_set_forward_page_func(arg0, arg1, arg2, arg3);
}

void Pure_gtk_assistant_set_page_type(GtkAssistant* arg0, GtkWidget* arg1, unsigned int arg2)
{
  return gtk_assistant_set_page_type(arg0, arg1, arg2);
}

unsigned int Pure_gtk_assistant_get_page_type(GtkAssistant* arg0, GtkWidget* arg1)
{
  return gtk_assistant_get_page_type(arg0, arg1);
}

void Pure_gtk_assistant_set_page_title(GtkAssistant* arg0, GtkWidget* arg1, char const* arg2)
{
  return gtk_assistant_set_page_title(arg0, arg1, arg2);
}

char const* Pure_gtk_assistant_get_page_title(GtkAssistant* arg0, GtkWidget* arg1)
{
  return gtk_assistant_get_page_title(arg0, arg1);
}

void Pure_gtk_assistant_set_page_header_image(GtkAssistant* arg0, GtkWidget* arg1, GdkPixbuf* arg2)
{
  return gtk_assistant_set_page_header_image(arg0, arg1, arg2);
}

GdkPixbuf* Pure_gtk_assistant_get_page_header_image(GtkAssistant* arg0, GtkWidget* arg1)
{
  return gtk_assistant_get_page_header_image(arg0, arg1);
}

void Pure_gtk_assistant_set_page_side_image(GtkAssistant* arg0, GtkWidget* arg1, GdkPixbuf* arg2)
{
  return gtk_assistant_set_page_side_image(arg0, arg1, arg2);
}

GdkPixbuf* Pure_gtk_assistant_get_page_side_image(GtkAssistant* arg0, GtkWidget* arg1)
{
  return gtk_assistant_get_page_side_image(arg0, arg1);
}

void Pure_gtk_assistant_set_page_complete(GtkAssistant* arg0, GtkWidget* arg1, int arg2)
{
  return gtk_assistant_set_page_complete(arg0, arg1, arg2);
}

int Pure_gtk_assistant_get_page_complete(GtkAssistant* arg0, GtkWidget* arg1)
{
  return gtk_assistant_get_page_complete(arg0, arg1);
}

void Pure_gtk_assistant_add_action_widget(GtkAssistant* arg0, GtkWidget* arg1)
{
  return gtk_assistant_add_action_widget(arg0, arg1);
}

void Pure_gtk_assistant_remove_action_widget(GtkAssistant* arg0, GtkWidget* arg1)
{
  return gtk_assistant_remove_action_widget(arg0, arg1);
}

void Pure_gtk_assistant_update_buttons_state(GtkAssistant* arg0)
{
  return gtk_assistant_update_buttons_state(arg0);
}

unsigned long Pure_gtk_box_get_type()
{
  return gtk_box_get_type();
}

void Pure_gtk_box_pack_start(GtkBox* arg0, GtkWidget* arg1, int arg2, int arg3, unsigned int arg4)
{
  return gtk_box_pack_start(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_box_pack_end(GtkBox* arg0, GtkWidget* arg1, int arg2, int arg3, unsigned int arg4)
{
  return gtk_box_pack_end(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_box_pack_start_defaults(GtkBox* arg0, GtkWidget* arg1)
{
  return gtk_box_pack_start_defaults(arg0, arg1);
}

void Pure_gtk_box_pack_end_defaults(GtkBox* arg0, GtkWidget* arg1)
{
  return gtk_box_pack_end_defaults(arg0, arg1);
}

void Pure_gtk_box_set_homogeneous(GtkBox* arg0, int arg1)
{
  return gtk_box_set_homogeneous(arg0, arg1);
}

int Pure_gtk_box_get_homogeneous(GtkBox* arg0)
{
  return gtk_box_get_homogeneous(arg0);
}

void Pure_gtk_box_set_spacing(GtkBox* arg0, int arg1)
{
  return gtk_box_set_spacing(arg0, arg1);
}

int Pure_gtk_box_get_spacing(GtkBox* arg0)
{
  return gtk_box_get_spacing(arg0);
}

void Pure_gtk_box_reorder_child(GtkBox* arg0, GtkWidget* arg1, int arg2)
{
  return gtk_box_reorder_child(arg0, arg1, arg2);
}

void Pure_gtk_box_query_child_packing(GtkBox* arg0, GtkWidget* arg1, int* arg2, int* arg3, unsigned int* arg4, unsigned int* arg5)
{
  return gtk_box_query_child_packing(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_box_set_child_packing(GtkBox* arg0, GtkWidget* arg1, int arg2, int arg3, unsigned int arg4, unsigned int arg5)
{
  return gtk_box_set_child_packing(arg0, arg1, arg2, arg3, arg4, arg5);
}

unsigned long Pure_gtk_button_box_get_type()
{
  return gtk_button_box_get_type();
}

unsigned int Pure_gtk_button_box_get_layout(GtkButtonBox* arg0)
{
  return gtk_button_box_get_layout(arg0);
}

void Pure_gtk_button_box_set_layout(GtkButtonBox* arg0, unsigned int arg1)
{
  return gtk_button_box_set_layout(arg0, arg1);
}

int Pure_gtk_button_box_get_child_secondary(GtkButtonBox* arg0, GtkWidget* arg1)
{
  return gtk_button_box_get_child_secondary(arg0, arg1);
}

void Pure_gtk_button_box_set_child_secondary(GtkButtonBox* arg0, GtkWidget* arg1, int arg2)
{
  return gtk_button_box_set_child_secondary(arg0, arg1, arg2);
}

void Pure_gtk_button_box_set_child_size(GtkButtonBox* arg0, int arg1, int arg2)
{
  return gtk_button_box_set_child_size(arg0, arg1, arg2);
}

void Pure_gtk_button_box_set_child_ipadding(GtkButtonBox* arg0, int arg1, int arg2)
{
  return gtk_button_box_set_child_ipadding(arg0, arg1, arg2);
}

void Pure_gtk_button_box_get_child_size(GtkButtonBox* arg0, int* arg1, int* arg2)
{
  return gtk_button_box_get_child_size(arg0, arg1, arg2);
}

void Pure_gtk_button_box_get_child_ipadding(GtkButtonBox* arg0, int* arg1, int* arg2)
{
  return gtk_button_box_get_child_ipadding(arg0, arg1, arg2);
}

GtkBindingSet* Pure_gtk_binding_set_new(char const* arg0)
{
  return gtk_binding_set_new(arg0);
}

GtkBindingSet* Pure_gtk_binding_set_by_class(void* arg0)
{
  return gtk_binding_set_by_class(arg0);
}

GtkBindingSet* Pure_gtk_binding_set_find(char const* arg0)
{
  return gtk_binding_set_find(arg0);
}

int Pure_gtk_bindings_activate(GtkObject* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_bindings_activate(arg0, arg1, arg2);
}

int Pure_gtk_bindings_activate_event(GtkObject* arg0, GdkEventKey* arg1)
{
  return gtk_bindings_activate_event(arg0, arg1);
}

int Pure_gtk_binding_set_activate(GtkBindingSet* arg0, unsigned int arg1, unsigned int arg2, GtkObject* arg3)
{
  return gtk_binding_set_activate(arg0, arg1, arg2, arg3);
}

void Pure_gtk_binding_entry_clear(GtkBindingSet* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_binding_entry_clear(arg0, arg1, arg2);
}

void Pure_gtk_binding_entry_add_signall(GtkBindingSet* arg0, unsigned int arg1, unsigned int arg2, char const* arg3, GSList* arg4)
{
  return gtk_binding_entry_add_signall(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_gtk_binding_parse_binding(GScanner* arg0)
{
  return gtk_binding_parse_binding(arg0);
}

void Pure_gtk_binding_entry_skip(GtkBindingSet* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_binding_entry_skip(arg0, arg1, arg2);
}

void Pure_gtk_binding_entry_add_signal(GtkBindingSet* arg0, unsigned int arg1, unsigned int arg2, char const* arg3, unsigned int arg4)
{
  return gtk_binding_entry_add_signal(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_binding_entry_remove(GtkBindingSet* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_binding_entry_remove(arg0, arg1, arg2);
}

void Pure_gtk_binding_set_add_path(GtkBindingSet* arg0, unsigned int arg1, char const* arg2, unsigned int arg3)
{
  return gtk_binding_set_add_path(arg0, arg1, arg2, arg3);
}

unsigned int Pure_gtk_builder_error_quark()
{
  return gtk_builder_error_quark();
}

unsigned long Pure_gtk_builder_get_type()
{
  return gtk_builder_get_type();
}

GtkBuilder* Pure_gtk_builder_new()
{
  return gtk_builder_new();
}

unsigned int Pure_gtk_builder_add_from_file(GtkBuilder* arg0, char const* arg1, GError** arg2)
{
  return gtk_builder_add_from_file(arg0, arg1, arg2);
}

unsigned int Pure_gtk_builder_add_from_string(GtkBuilder* arg0, char const* arg1, unsigned long arg2, GError** arg3)
{
  return gtk_builder_add_from_string(arg0, arg1, arg2, arg3);
}

unsigned int Pure_gtk_builder_add_objects_from_file(GtkBuilder* arg0, char const* arg1, char** arg2, GError** arg3)
{
  return gtk_builder_add_objects_from_file(arg0, arg1, arg2, arg3);
}

unsigned int Pure_gtk_builder_add_objects_from_string(GtkBuilder* arg0, char const* arg1, unsigned long arg2, char** arg3, GError** arg4)
{
  return gtk_builder_add_objects_from_string(arg0, arg1, arg2, arg3, arg4);
}

GObject* Pure_gtk_builder_get_object(GtkBuilder* arg0, char const* arg1)
{
  return gtk_builder_get_object(arg0, arg1);
}

GSList* Pure_gtk_builder_get_objects(GtkBuilder* arg0)
{
  return gtk_builder_get_objects(arg0);
}

void Pure_gtk_builder_connect_signals(GtkBuilder* arg0, void* arg1)
{
  return gtk_builder_connect_signals(arg0, arg1);
}

void Pure_gtk_builder_connect_signals_full(GtkBuilder* arg0, void* arg1, void* arg2)
{
  return gtk_builder_connect_signals_full(arg0, arg1, arg2);
}

void Pure_gtk_builder_set_translation_domain(GtkBuilder* arg0, char const* arg1)
{
  return gtk_builder_set_translation_domain(arg0, arg1);
}

char const* Pure_gtk_builder_get_translation_domain(GtkBuilder* arg0)
{
  return gtk_builder_get_translation_domain(arg0);
}

unsigned long Pure_gtk_builder_get_type_from_name(GtkBuilder* arg0, char const* arg1)
{
  return gtk_builder_get_type_from_name(arg0, arg1);
}

int Pure_gtk_builder_value_from_string(GtkBuilder* arg0, GParamSpec* arg1, char const* arg2, GValue* arg3, GError** arg4)
{
  return gtk_builder_value_from_string(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gtk_builder_value_from_string_type(GtkBuilder* arg0, unsigned long arg1, char const* arg2, GValue* arg3, GError** arg4)
{
  return gtk_builder_value_from_string_type(arg0, arg1, arg2, arg3, arg4);
}

unsigned long Pure_gtk_buildable_get_type()
{
  return gtk_buildable_get_type();
}

void Pure_gtk_buildable_set_name(GtkBuildable* arg0, char const* arg1)
{
  return gtk_buildable_set_name(arg0, arg1);
}

char const* Pure_gtk_buildable_get_name(GtkBuildable* arg0)
{
  return gtk_buildable_get_name(arg0);
}

void Pure_gtk_buildable_add_child(GtkBuildable* arg0, GtkBuilder* arg1, GObject* arg2, char const* arg3)
{
  return gtk_buildable_add_child(arg0, arg1, arg2, arg3);
}

void Pure_gtk_buildable_set_buildable_property(GtkBuildable* arg0, GtkBuilder* arg1, char const* arg2, GValue const* arg3)
{
  return gtk_buildable_set_buildable_property(arg0, arg1, arg2, arg3);
}

GObject* Pure_gtk_buildable_construct_child(GtkBuildable* arg0, GtkBuilder* arg1, char const* arg2)
{
  return gtk_buildable_construct_child(arg0, arg1, arg2);
}

int Pure_gtk_buildable_custom_tag_start(GtkBuildable* arg0, GtkBuilder* arg1, GObject* arg2, char const* arg3, GMarkupParser* arg4, void** arg5)
{
  return gtk_buildable_custom_tag_start(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_buildable_custom_tag_end(GtkBuildable* arg0, GtkBuilder* arg1, GObject* arg2, char const* arg3, void** arg4)
{
  return gtk_buildable_custom_tag_end(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_buildable_custom_finished(GtkBuildable* arg0, GtkBuilder* arg1, GObject* arg2, char const* arg3, void* arg4)
{
  return gtk_buildable_custom_finished(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_buildable_parser_finished(GtkBuildable* arg0, GtkBuilder* arg1)
{
  return gtk_buildable_parser_finished(arg0, arg1);
}

GObject* Pure_gtk_buildable_get_internal_child(GtkBuildable* arg0, GtkBuilder* arg1, char const* arg2)
{
  return gtk_buildable_get_internal_child(arg0, arg1, arg2);
}

unsigned long Pure_gtk_image_get_type()
{
  return gtk_image_get_type();
}

GtkWidget* Pure_gtk_image_new()
{
  return gtk_image_new();
}

GtkWidget* Pure_gtk_image_new_from_pixmap(GdkPixmap* arg0, GdkBitmap* arg1)
{
  return gtk_image_new_from_pixmap(arg0, arg1);
}

GtkWidget* Pure_gtk_image_new_from_image(GdkImage* arg0, GdkBitmap* arg1)
{
  return gtk_image_new_from_image(arg0, arg1);
}

GtkWidget* Pure_gtk_image_new_from_file(char const* arg0)
{
  return gtk_image_new_from_file(arg0);
}

GtkWidget* Pure_gtk_image_new_from_pixbuf(GdkPixbuf* arg0)
{
  return gtk_image_new_from_pixbuf(arg0);
}

GtkWidget* Pure_gtk_image_new_from_stock(char const* arg0, unsigned int arg1)
{
  return gtk_image_new_from_stock(arg0, arg1);
}

GtkWidget* Pure_gtk_image_new_from_icon_set(GtkIconSet* arg0, unsigned int arg1)
{
  return gtk_image_new_from_icon_set(arg0, arg1);
}

GtkWidget* Pure_gtk_image_new_from_animation(GdkPixbufAnimation* arg0)
{
  return gtk_image_new_from_animation(arg0);
}

GtkWidget* Pure_gtk_image_new_from_icon_name(char const* arg0, unsigned int arg1)
{
  return gtk_image_new_from_icon_name(arg0, arg1);
}

GtkWidget* Pure_gtk_image_new_from_gicon(GIcon* arg0, unsigned int arg1)
{
  return gtk_image_new_from_gicon(arg0, arg1);
}

void Pure_gtk_image_clear(GtkImage* arg0)
{
  return gtk_image_clear(arg0);
}

void Pure_gtk_image_set_from_pixmap(GtkImage* arg0, GdkPixmap* arg1, GdkBitmap* arg2)
{
  return gtk_image_set_from_pixmap(arg0, arg1, arg2);
}

void Pure_gtk_image_set_from_image(GtkImage* arg0, GdkImage* arg1, GdkBitmap* arg2)
{
  return gtk_image_set_from_image(arg0, arg1, arg2);
}

void Pure_gtk_image_set_from_file(GtkImage* arg0, char const* arg1)
{
  return gtk_image_set_from_file(arg0, arg1);
}

void Pure_gtk_image_set_from_pixbuf(GtkImage* arg0, GdkPixbuf* arg1)
{
  return gtk_image_set_from_pixbuf(arg0, arg1);
}

void Pure_gtk_image_set_from_stock(GtkImage* arg0, char const* arg1, unsigned int arg2)
{
  return gtk_image_set_from_stock(arg0, arg1, arg2);
}

void Pure_gtk_image_set_from_icon_set(GtkImage* arg0, GtkIconSet* arg1, unsigned int arg2)
{
  return gtk_image_set_from_icon_set(arg0, arg1, arg2);
}

void Pure_gtk_image_set_from_animation(GtkImage* arg0, GdkPixbufAnimation* arg1)
{
  return gtk_image_set_from_animation(arg0, arg1);
}

void Pure_gtk_image_set_from_icon_name(GtkImage* arg0, char const* arg1, unsigned int arg2)
{
  return gtk_image_set_from_icon_name(arg0, arg1, arg2);
}

void Pure_gtk_image_set_from_gicon(GtkImage* arg0, GIcon* arg1, unsigned int arg2)
{
  return gtk_image_set_from_gicon(arg0, arg1, arg2);
}

void Pure_gtk_image_set_pixel_size(GtkImage* arg0, int arg1)
{
  return gtk_image_set_pixel_size(arg0, arg1);
}

unsigned int Pure_gtk_image_get_storage_type(GtkImage* arg0)
{
  return gtk_image_get_storage_type(arg0);
}

void Pure_gtk_image_get_pixmap(GtkImage* arg0, GdkPixmap** arg1, GdkBitmap** arg2)
{
  return gtk_image_get_pixmap(arg0, arg1, arg2);
}

void Pure_gtk_image_get_image(GtkImage* arg0, GdkImage** arg1, GdkBitmap** arg2)
{
  return gtk_image_get_image(arg0, arg1, arg2);
}

GdkPixbuf* Pure_gtk_image_get_pixbuf(GtkImage* arg0)
{
  return gtk_image_get_pixbuf(arg0);
}

void Pure_gtk_image_get_stock(GtkImage* arg0, char** arg1, unsigned int* arg2)
{
  return gtk_image_get_stock(arg0, arg1, arg2);
}

void Pure_gtk_image_get_icon_set(GtkImage* arg0, GtkIconSet** arg1, unsigned int* arg2)
{
  return gtk_image_get_icon_set(arg0, arg1, arg2);
}

GdkPixbufAnimation* Pure_gtk_image_get_animation(GtkImage* arg0)
{
  return gtk_image_get_animation(arg0);
}

void Pure_gtk_image_get_icon_name(GtkImage* arg0, char const** arg1, unsigned int* arg2)
{
  return gtk_image_get_icon_name(arg0, arg1, arg2);
}

void Pure_gtk_image_get_gicon(GtkImage* arg0, GIcon** arg1, unsigned int* arg2)
{
  return gtk_image_get_gicon(arg0, arg1, arg2);
}

int Pure_gtk_image_get_pixel_size(GtkImage* arg0)
{
  return gtk_image_get_pixel_size(arg0);
}

void Pure_gtk_image_set(GtkImage* arg0, GdkImage* arg1, GdkBitmap* arg2)
{
  return gtk_image_set(arg0, arg1, arg2);
}

void Pure_gtk_image_get(GtkImage* arg0, GdkImage** arg1, GdkBitmap** arg2)
{
  return gtk_image_get(arg0, arg1, arg2);
}

unsigned long Pure_gtk_button_get_type()
{
  return gtk_button_get_type();
}

GtkWidget* Pure_gtk_button_new()
{
  return gtk_button_new();
}

GtkWidget* Pure_gtk_button_new_with_label(char const* arg0)
{
  return gtk_button_new_with_label(arg0);
}

GtkWidget* Pure_gtk_button_new_from_stock(char const* arg0)
{
  return gtk_button_new_from_stock(arg0);
}

GtkWidget* Pure_gtk_button_new_with_mnemonic(char const* arg0)
{
  return gtk_button_new_with_mnemonic(arg0);
}

void Pure_gtk_button_pressed(GtkButton* arg0)
{
  return gtk_button_pressed(arg0);
}

void Pure_gtk_button_released(GtkButton* arg0)
{
  return gtk_button_released(arg0);
}

void Pure_gtk_button_clicked(GtkButton* arg0)
{
  return gtk_button_clicked(arg0);
}

void Pure_gtk_button_enter(GtkButton* arg0)
{
  return gtk_button_enter(arg0);
}

void Pure_gtk_button_leave(GtkButton* arg0)
{
  return gtk_button_leave(arg0);
}

void Pure_gtk_button_set_relief(GtkButton* arg0, unsigned int arg1)
{
  return gtk_button_set_relief(arg0, arg1);
}

unsigned int Pure_gtk_button_get_relief(GtkButton* arg0)
{
  return gtk_button_get_relief(arg0);
}

void Pure_gtk_button_set_label(GtkButton* arg0, char const* arg1)
{
  return gtk_button_set_label(arg0, arg1);
}

char const* Pure_gtk_button_get_label(GtkButton* arg0)
{
  return gtk_button_get_label(arg0);
}

void Pure_gtk_button_set_use_underline(GtkButton* arg0, int arg1)
{
  return gtk_button_set_use_underline(arg0, arg1);
}

int Pure_gtk_button_get_use_underline(GtkButton* arg0)
{
  return gtk_button_get_use_underline(arg0);
}

void Pure_gtk_button_set_use_stock(GtkButton* arg0, int arg1)
{
  return gtk_button_set_use_stock(arg0, arg1);
}

int Pure_gtk_button_get_use_stock(GtkButton* arg0)
{
  return gtk_button_get_use_stock(arg0);
}

void Pure_gtk_button_set_focus_on_click(GtkButton* arg0, int arg1)
{
  return gtk_button_set_focus_on_click(arg0, arg1);
}

int Pure_gtk_button_get_focus_on_click(GtkButton* arg0)
{
  return gtk_button_get_focus_on_click(arg0);
}

void Pure_gtk_button_set_alignment(GtkButton* arg0, float arg1, float arg2)
{
  return gtk_button_set_alignment(arg0, arg1, arg2);
}

void Pure_gtk_button_get_alignment(GtkButton* arg0, float* arg1, float* arg2)
{
  return gtk_button_get_alignment(arg0, arg1, arg2);
}

void Pure_gtk_button_set_image(GtkButton* arg0, GtkWidget* arg1)
{
  return gtk_button_set_image(arg0, arg1);
}

GtkWidget* Pure_gtk_button_get_image(GtkButton* arg0)
{
  return gtk_button_get_image(arg0);
}

void Pure_gtk_button_set_image_position(GtkButton* arg0, unsigned int arg1)
{
  return gtk_button_set_image_position(arg0, arg1);
}

unsigned int Pure_gtk_button_get_image_position(GtkButton* arg0)
{
  return gtk_button_get_image_position(arg0);
}

void Pure_gtk_marshal_BOOLEAN__VOID(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_BOOLEAN__VOID(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_BOOLEAN__POINTER(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_BOOLEAN__POINTER(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_BOOLEAN__POINTER_POINTER_INT_INT(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_BOOLEAN__POINTER_POINTER_INT_INT(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_BOOLEAN__POINTER_INT_INT(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_BOOLEAN__POINTER_INT_INT(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_BOOLEAN__POINTER_INT_INT_UINT(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_BOOLEAN__POINTER_INT_INT_UINT(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_BOOLEAN__POINTER_STRING_STRING_POINTER(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_BOOLEAN__POINTER_STRING_STRING_POINTER(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_ENUM__ENUM(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_ENUM__ENUM(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_INT__POINTER(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_INT__POINTER(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_INT__POINTER_CHAR_CHAR(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_INT__POINTER_CHAR_CHAR(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__ENUM_FLOAT(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__ENUM_FLOAT(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__ENUM_FLOAT_BOOLEAN(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__ENUM_FLOAT_BOOLEAN(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__INT_INT(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__INT_INT(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__INT_INT_POINTER(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__INT_INT_POINTER(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__POINTER_INT(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__POINTER_INT(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__POINTER_POINTER(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__POINTER_POINTER(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__POINTER_POINTER_POINTER(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__POINTER_POINTER_POINTER(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__POINTER_STRING_STRING(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__POINTER_STRING_STRING(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__POINTER_UINT(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__POINTER_UINT(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__POINTER_UINT_ENUM(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__POINTER_UINT_ENUM(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__POINTER_POINTER_UINT_UINT(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__POINTER_POINTER_UINT_UINT(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__POINTER_INT_INT_POINTER_UINT_UINT(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__POINTER_INT_INT_POINTER_UINT_UINT(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__POINTER_UINT_UINT(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__POINTER_UINT_UINT(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__STRING_INT_POINTER(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__STRING_INT_POINTER(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__UINT_POINTER_UINT_ENUM_ENUM_POINTER(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__UINT_POINTER_UINT_ENUM_ENUM_POINTER(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__UINT_POINTER_UINT_UINT_ENUM(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__UINT_POINTER_UINT_UINT_ENUM(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_marshal_VOID__UINT_STRING(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return gtk_marshal_VOID__UINT_STRING(arg0, arg1, arg2, arg3, arg4, arg5);
}

unsigned int Pure_gtk_signal_newv(char const* arg0, unsigned int arg1, unsigned long arg2, unsigned int arg3, void* arg4, unsigned long arg5, unsigned int arg6, unsigned long* arg7)
{
  return gtk_signal_newv(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

unsigned int Pure_gtk_signal_new(char const* arg0, unsigned int arg1, unsigned long arg2, unsigned int arg3, void* arg4, unsigned long arg5, unsigned int arg6)
{
  return gtk_signal_new(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_signal_emit_stop_by_name(GtkObject* arg0, char const* arg1)
{
  return gtk_signal_emit_stop_by_name(arg0, arg1);
}

void Pure_gtk_signal_connect_object_while_alive(GtkObject* arg0, char const* arg1, void* arg2, GtkObject* arg3)
{
  return gtk_signal_connect_object_while_alive(arg0, arg1, arg2, arg3);
}

void Pure_gtk_signal_connect_while_alive(GtkObject* arg0, char const* arg1, void* arg2, void* arg3, GtkObject* arg4)
{
  return gtk_signal_connect_while_alive(arg0, arg1, arg2, arg3, arg4);
}

unsigned long Pure_gtk_signal_connect_full(GtkObject* arg0, char const* arg1, void* arg2, void* arg3, void* arg4, void* arg5, int arg6, int arg7)
{
  return gtk_signal_connect_full(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_gtk_signal_emitv(GtkObject* arg0, unsigned int arg1, GtkArg* arg2)
{
  return gtk_signal_emitv(arg0, arg1, arg2);
}

void Pure_gtk_signal_emit(GtkObject* arg0, unsigned int arg1)
{
  return gtk_signal_emit(arg0, arg1);
}

void Pure_gtk_signal_emit_by_name(GtkObject* arg0, char const* arg1)
{
  return gtk_signal_emit_by_name(arg0, arg1);
}

void Pure_gtk_signal_emitv_by_name(GtkObject* arg0, char const* arg1, GtkArg* arg2)
{
  return gtk_signal_emitv_by_name(arg0, arg1, arg2);
}

void Pure_gtk_signal_compat_matched(GtkObject* arg0, void* arg1, void* arg2, unsigned int arg3, unsigned int arg4)
{
  return gtk_signal_compat_matched(arg0, arg1, arg2, arg3, arg4);
}

unsigned long Pure_gtk_calendar_get_type()
{
  return gtk_calendar_get_type();
}

GtkWidget* Pure_gtk_calendar_new()
{
  return gtk_calendar_new();
}

int Pure_gtk_calendar_select_month(GtkCalendar* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_calendar_select_month(arg0, arg1, arg2);
}

void Pure_gtk_calendar_select_day(GtkCalendar* arg0, unsigned int arg1)
{
  return gtk_calendar_select_day(arg0, arg1);
}

int Pure_gtk_calendar_mark_day(GtkCalendar* arg0, unsigned int arg1)
{
  return gtk_calendar_mark_day(arg0, arg1);
}

int Pure_gtk_calendar_unmark_day(GtkCalendar* arg0, unsigned int arg1)
{
  return gtk_calendar_unmark_day(arg0, arg1);
}

void Pure_gtk_calendar_clear_marks(GtkCalendar* arg0)
{
  return gtk_calendar_clear_marks(arg0);
}

void Pure_gtk_calendar_set_display_options(GtkCalendar* arg0, unsigned int arg1)
{
  return gtk_calendar_set_display_options(arg0, arg1);
}

unsigned int Pure_gtk_calendar_get_display_options(GtkCalendar* arg0)
{
  return gtk_calendar_get_display_options(arg0);
}

void Pure_gtk_calendar_display_options(GtkCalendar* arg0, unsigned int arg1)
{
  return gtk_calendar_display_options(arg0, arg1);
}

void Pure_gtk_calendar_get_date(GtkCalendar* arg0, unsigned int* arg1, unsigned int* arg2, unsigned int* arg3)
{
  return gtk_calendar_get_date(arg0, arg1, arg2, arg3);
}

void Pure_gtk_calendar_set_detail_func(GtkCalendar* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_calendar_set_detail_func(arg0, arg1, arg2, arg3);
}

void Pure_gtk_calendar_set_detail_width_chars(GtkCalendar* arg0, int arg1)
{
  return gtk_calendar_set_detail_width_chars(arg0, arg1);
}

void Pure_gtk_calendar_set_detail_height_rows(GtkCalendar* arg0, int arg1)
{
  return gtk_calendar_set_detail_height_rows(arg0, arg1);
}

int Pure_gtk_calendar_get_detail_width_chars(GtkCalendar* arg0)
{
  return gtk_calendar_get_detail_width_chars(arg0);
}

int Pure_gtk_calendar_get_detail_height_rows(GtkCalendar* arg0)
{
  return gtk_calendar_get_detail_height_rows(arg0);
}

void Pure_gtk_calendar_freeze(GtkCalendar* arg0)
{
  return gtk_calendar_freeze(arg0);
}

void Pure_gtk_calendar_thaw(GtkCalendar* arg0)
{
  return gtk_calendar_thaw(arg0);
}

unsigned long Pure_gtk_cell_editable_get_type()
{
  return gtk_cell_editable_get_type();
}

void Pure_gtk_cell_editable_start_editing(GtkCellEditable* arg0, GdkEvent* arg1)
{
  return gtk_cell_editable_start_editing(arg0, arg1);
}

void Pure_gtk_cell_editable_editing_done(GtkCellEditable* arg0)
{
  return gtk_cell_editable_editing_done(arg0);
}

void Pure_gtk_cell_editable_remove_widget(GtkCellEditable* arg0)
{
  return gtk_cell_editable_remove_widget(arg0);
}

unsigned long Pure_gtk_cell_renderer_get_type()
{
  return gtk_cell_renderer_get_type();
}

void Pure_gtk_cell_renderer_get_size(GtkCellRenderer* arg0, GtkWidget* arg1, GdkRectangle const* arg2, int* arg3, int* arg4, int* arg5, int* arg6)
{
  return gtk_cell_renderer_get_size(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_cell_renderer_render(GtkCellRenderer* arg0, GdkWindow* arg1, GtkWidget* arg2, GdkRectangle const* arg3, GdkRectangle const* arg4, GdkRectangle const* arg5, unsigned int arg6)
{
  return gtk_cell_renderer_render(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

int Pure_gtk_cell_renderer_activate(GtkCellRenderer* arg0, GdkEvent* arg1, GtkWidget* arg2, char const* arg3, GdkRectangle const* arg4, GdkRectangle const* arg5, unsigned int arg6)
{
  return gtk_cell_renderer_activate(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GtkCellEditable* Pure_gtk_cell_renderer_start_editing(GtkCellRenderer* arg0, GdkEvent* arg1, GtkWidget* arg2, char const* arg3, GdkRectangle const* arg4, GdkRectangle const* arg5, unsigned int arg6)
{
  return gtk_cell_renderer_start_editing(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_cell_renderer_set_fixed_size(GtkCellRenderer* arg0, int arg1, int arg2)
{
  return gtk_cell_renderer_set_fixed_size(arg0, arg1, arg2);
}

void Pure_gtk_cell_renderer_get_fixed_size(GtkCellRenderer* arg0, int* arg1, int* arg2)
{
  return gtk_cell_renderer_get_fixed_size(arg0, arg1, arg2);
}

void Pure_gtk_cell_renderer_editing_canceled(GtkCellRenderer* arg0)
{
  return gtk_cell_renderer_editing_canceled(arg0);
}

void Pure_gtk_cell_renderer_stop_editing(GtkCellRenderer* arg0, int arg1)
{
  return gtk_cell_renderer_stop_editing(arg0, arg1);
}

GtkTreePath* Pure_gtk_tree_path_new()
{
  return gtk_tree_path_new();
}

GtkTreePath* Pure_gtk_tree_path_new_from_string(char const* arg0)
{
  return gtk_tree_path_new_from_string(arg0);
}

GtkTreePath* Pure_gtk_tree_path_new_from_indices(int arg0)
{
  return gtk_tree_path_new_from_indices(arg0);
}

char* Pure_gtk_tree_path_to_string(GtkTreePath* arg0)
{
  return gtk_tree_path_to_string(arg0);
}

GtkTreePath* Pure_gtk_tree_path_new_first()
{
  return gtk_tree_path_new_first();
}

void Pure_gtk_tree_path_append_index(GtkTreePath* arg0, int arg1)
{
  return gtk_tree_path_append_index(arg0, arg1);
}

void Pure_gtk_tree_path_prepend_index(GtkTreePath* arg0, int arg1)
{
  return gtk_tree_path_prepend_index(arg0, arg1);
}

int Pure_gtk_tree_path_get_depth(GtkTreePath* arg0)
{
  return gtk_tree_path_get_depth(arg0);
}

int* Pure_gtk_tree_path_get_indices(GtkTreePath* arg0)
{
  return gtk_tree_path_get_indices(arg0);
}

void Pure_gtk_tree_path_free(GtkTreePath* arg0)
{
  return gtk_tree_path_free(arg0);
}

GtkTreePath* Pure_gtk_tree_path_copy(GtkTreePath const* arg0)
{
  return gtk_tree_path_copy(arg0);
}

unsigned long Pure_gtk_tree_path_get_type()
{
  return gtk_tree_path_get_type();
}

int Pure_gtk_tree_path_compare(GtkTreePath const* arg0, GtkTreePath const* arg1)
{
  return gtk_tree_path_compare(arg0, arg1);
}

void Pure_gtk_tree_path_next(GtkTreePath* arg0)
{
  return gtk_tree_path_next(arg0);
}

int Pure_gtk_tree_path_prev(GtkTreePath* arg0)
{
  return gtk_tree_path_prev(arg0);
}

int Pure_gtk_tree_path_up(GtkTreePath* arg0)
{
  return gtk_tree_path_up(arg0);
}

void Pure_gtk_tree_path_down(GtkTreePath* arg0)
{
  return gtk_tree_path_down(arg0);
}

int Pure_gtk_tree_path_is_ancestor(GtkTreePath* arg0, GtkTreePath* arg1)
{
  return gtk_tree_path_is_ancestor(arg0, arg1);
}

int Pure_gtk_tree_path_is_descendant(GtkTreePath* arg0, GtkTreePath* arg1)
{
  return gtk_tree_path_is_descendant(arg0, arg1);
}

unsigned long Pure_gtk_tree_row_reference_get_type()
{
  return gtk_tree_row_reference_get_type();
}

GtkTreeRowReference* Pure_gtk_tree_row_reference_new(GtkTreeModel* arg0, GtkTreePath* arg1)
{
  return gtk_tree_row_reference_new(arg0, arg1);
}

GtkTreeRowReference* Pure_gtk_tree_row_reference_new_proxy(GObject* arg0, GtkTreeModel* arg1, GtkTreePath* arg2)
{
  return gtk_tree_row_reference_new_proxy(arg0, arg1, arg2);
}

GtkTreePath* Pure_gtk_tree_row_reference_get_path(GtkTreeRowReference* arg0)
{
  return gtk_tree_row_reference_get_path(arg0);
}

GtkTreeModel* Pure_gtk_tree_row_reference_get_model(GtkTreeRowReference* arg0)
{
  return gtk_tree_row_reference_get_model(arg0);
}

int Pure_gtk_tree_row_reference_valid(GtkTreeRowReference* arg0)
{
  return gtk_tree_row_reference_valid(arg0);
}

GtkTreeRowReference* Pure_gtk_tree_row_reference_copy(GtkTreeRowReference* arg0)
{
  return gtk_tree_row_reference_copy(arg0);
}

void Pure_gtk_tree_row_reference_free(GtkTreeRowReference* arg0)
{
  return gtk_tree_row_reference_free(arg0);
}

void Pure_gtk_tree_row_reference_inserted(GObject* arg0, GtkTreePath* arg1)
{
  return gtk_tree_row_reference_inserted(arg0, arg1);
}

void Pure_gtk_tree_row_reference_deleted(GObject* arg0, GtkTreePath* arg1)
{
  return gtk_tree_row_reference_deleted(arg0, arg1);
}

void Pure_gtk_tree_row_reference_reordered(GObject* arg0, GtkTreePath* arg1, GtkTreeIter* arg2, int* arg3)
{
  return gtk_tree_row_reference_reordered(arg0, arg1, arg2, arg3);
}

GtkTreeIter* Pure_gtk_tree_iter_copy(GtkTreeIter* arg0)
{
  return gtk_tree_iter_copy(arg0);
}

void Pure_gtk_tree_iter_free(GtkTreeIter* arg0)
{
  return gtk_tree_iter_free(arg0);
}

unsigned long Pure_gtk_tree_iter_get_type()
{
  return gtk_tree_iter_get_type();
}

unsigned long Pure_gtk_tree_model_get_type()
{
  return gtk_tree_model_get_type();
}

unsigned int Pure_gtk_tree_model_get_flags(GtkTreeModel* arg0)
{
  return gtk_tree_model_get_flags(arg0);
}

int Pure_gtk_tree_model_get_n_columns(GtkTreeModel* arg0)
{
  return gtk_tree_model_get_n_columns(arg0);
}

unsigned long Pure_gtk_tree_model_get_column_type(GtkTreeModel* arg0, int arg1)
{
  return gtk_tree_model_get_column_type(arg0, arg1);
}

int Pure_gtk_tree_model_get_iter(GtkTreeModel* arg0, GtkTreeIter* arg1, GtkTreePath* arg2)
{
  return gtk_tree_model_get_iter(arg0, arg1, arg2);
}

int Pure_gtk_tree_model_get_iter_from_string(GtkTreeModel* arg0, GtkTreeIter* arg1, char const* arg2)
{
  return gtk_tree_model_get_iter_from_string(arg0, arg1, arg2);
}

char* Pure_gtk_tree_model_get_string_from_iter(GtkTreeModel* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_model_get_string_from_iter(arg0, arg1);
}

int Pure_gtk_tree_model_get_iter_first(GtkTreeModel* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_model_get_iter_first(arg0, arg1);
}

GtkTreePath* Pure_gtk_tree_model_get_path(GtkTreeModel* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_model_get_path(arg0, arg1);
}

void Pure_gtk_tree_model_get_value(GtkTreeModel* arg0, GtkTreeIter* arg1, int arg2, GValue* arg3)
{
  return gtk_tree_model_get_value(arg0, arg1, arg2, arg3);
}

int Pure_gtk_tree_model_iter_next(GtkTreeModel* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_model_iter_next(arg0, arg1);
}

int Pure_gtk_tree_model_iter_children(GtkTreeModel* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_model_iter_children(arg0, arg1, arg2);
}

int Pure_gtk_tree_model_iter_has_child(GtkTreeModel* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_model_iter_has_child(arg0, arg1);
}

int Pure_gtk_tree_model_iter_n_children(GtkTreeModel* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_model_iter_n_children(arg0, arg1);
}

int Pure_gtk_tree_model_iter_nth_child(GtkTreeModel* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2, int arg3)
{
  return gtk_tree_model_iter_nth_child(arg0, arg1, arg2, arg3);
}

int Pure_gtk_tree_model_iter_parent(GtkTreeModel* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_model_iter_parent(arg0, arg1, arg2);
}

void Pure_gtk_tree_model_ref_node(GtkTreeModel* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_model_ref_node(arg0, arg1);
}

void Pure_gtk_tree_model_unref_node(GtkTreeModel* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_model_unref_node(arg0, arg1);
}

void Pure_gtk_tree_model_get(GtkTreeModel* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_model_get(arg0, arg1);
}

void Pure_gtk_tree_model_get_valist(GtkTreeModel* arg0, GtkTreeIter* arg1, void* arg2)
{
  return gtk_tree_model_get_valist(arg0, arg1, arg2);
}

void Pure_gtk_tree_model_foreach(GtkTreeModel* arg0, void* arg1, void* arg2)
{
  return gtk_tree_model_foreach(arg0, arg1, arg2);
}

void Pure_gtk_tree_model_row_changed(GtkTreeModel* arg0, GtkTreePath* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_model_row_changed(arg0, arg1, arg2);
}

void Pure_gtk_tree_model_row_inserted(GtkTreeModel* arg0, GtkTreePath* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_model_row_inserted(arg0, arg1, arg2);
}

void Pure_gtk_tree_model_row_has_child_toggled(GtkTreeModel* arg0, GtkTreePath* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_model_row_has_child_toggled(arg0, arg1, arg2);
}

void Pure_gtk_tree_model_row_deleted(GtkTreeModel* arg0, GtkTreePath* arg1)
{
  return gtk_tree_model_row_deleted(arg0, arg1);
}

void Pure_gtk_tree_model_rows_reordered(GtkTreeModel* arg0, GtkTreePath* arg1, GtkTreeIter* arg2, int* arg3)
{
  return gtk_tree_model_rows_reordered(arg0, arg1, arg2, arg3);
}

unsigned long Pure_gtk_tree_sortable_get_type()
{
  return gtk_tree_sortable_get_type();
}

void Pure_gtk_tree_sortable_sort_column_changed(GtkTreeSortable* arg0)
{
  return gtk_tree_sortable_sort_column_changed(arg0);
}

int Pure_gtk_tree_sortable_get_sort_column_id(GtkTreeSortable* arg0, int* arg1, unsigned int* arg2)
{
  return gtk_tree_sortable_get_sort_column_id(arg0, arg1, arg2);
}

void Pure_gtk_tree_sortable_set_sort_column_id(GtkTreeSortable* arg0, int arg1, unsigned int arg2)
{
  return gtk_tree_sortable_set_sort_column_id(arg0, arg1, arg2);
}

void Pure_gtk_tree_sortable_set_sort_func(GtkTreeSortable* arg0, int arg1, void* arg2, void* arg3, void* arg4)
{
  return gtk_tree_sortable_set_sort_func(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_tree_sortable_set_default_sort_func(GtkTreeSortable* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_tree_sortable_set_default_sort_func(arg0, arg1, arg2, arg3);
}

int Pure_gtk_tree_sortable_has_default_sort_func(GtkTreeSortable* arg0)
{
  return gtk_tree_sortable_has_default_sort_func(arg0);
}

unsigned long Pure_gtk_tree_view_column_get_type()
{
  return gtk_tree_view_column_get_type();
}

GtkTreeViewColumn* Pure_gtk_tree_view_column_new()
{
  return gtk_tree_view_column_new();
}

GtkTreeViewColumn* Pure_gtk_tree_view_column_new_with_attributes(char const* arg0, GtkCellRenderer* arg1)
{
  return gtk_tree_view_column_new_with_attributes(arg0, arg1);
}

void Pure_gtk_tree_view_column_pack_start(GtkTreeViewColumn* arg0, GtkCellRenderer* arg1, int arg2)
{
  return gtk_tree_view_column_pack_start(arg0, arg1, arg2);
}

void Pure_gtk_tree_view_column_pack_end(GtkTreeViewColumn* arg0, GtkCellRenderer* arg1, int arg2)
{
  return gtk_tree_view_column_pack_end(arg0, arg1, arg2);
}

void Pure_gtk_tree_view_column_clear(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_clear(arg0);
}

GList* Pure_gtk_tree_view_column_get_cell_renderers(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_cell_renderers(arg0);
}

void Pure_gtk_tree_view_column_add_attribute(GtkTreeViewColumn* arg0, GtkCellRenderer* arg1, char const* arg2, int arg3)
{
  return gtk_tree_view_column_add_attribute(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_view_column_set_attributes(GtkTreeViewColumn* arg0, GtkCellRenderer* arg1)
{
  return gtk_tree_view_column_set_attributes(arg0, arg1);
}

void Pure_gtk_tree_view_column_set_cell_data_func(GtkTreeViewColumn* arg0, GtkCellRenderer* arg1, void* arg2, void* arg3, void* arg4)
{
  return gtk_tree_view_column_set_cell_data_func(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_tree_view_column_clear_attributes(GtkTreeViewColumn* arg0, GtkCellRenderer* arg1)
{
  return gtk_tree_view_column_clear_attributes(arg0, arg1);
}

void Pure_gtk_tree_view_column_set_spacing(GtkTreeViewColumn* arg0, int arg1)
{
  return gtk_tree_view_column_set_spacing(arg0, arg1);
}

int Pure_gtk_tree_view_column_get_spacing(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_spacing(arg0);
}

void Pure_gtk_tree_view_column_set_visible(GtkTreeViewColumn* arg0, int arg1)
{
  return gtk_tree_view_column_set_visible(arg0, arg1);
}

int Pure_gtk_tree_view_column_get_visible(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_visible(arg0);
}

void Pure_gtk_tree_view_column_set_resizable(GtkTreeViewColumn* arg0, int arg1)
{
  return gtk_tree_view_column_set_resizable(arg0, arg1);
}

int Pure_gtk_tree_view_column_get_resizable(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_resizable(arg0);
}

void Pure_gtk_tree_view_column_set_sizing(GtkTreeViewColumn* arg0, unsigned int arg1)
{
  return gtk_tree_view_column_set_sizing(arg0, arg1);
}

unsigned int Pure_gtk_tree_view_column_get_sizing(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_sizing(arg0);
}

int Pure_gtk_tree_view_column_get_width(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_width(arg0);
}

int Pure_gtk_tree_view_column_get_fixed_width(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_fixed_width(arg0);
}

void Pure_gtk_tree_view_column_set_fixed_width(GtkTreeViewColumn* arg0, int arg1)
{
  return gtk_tree_view_column_set_fixed_width(arg0, arg1);
}

void Pure_gtk_tree_view_column_set_min_width(GtkTreeViewColumn* arg0, int arg1)
{
  return gtk_tree_view_column_set_min_width(arg0, arg1);
}

int Pure_gtk_tree_view_column_get_min_width(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_min_width(arg0);
}

void Pure_gtk_tree_view_column_set_max_width(GtkTreeViewColumn* arg0, int arg1)
{
  return gtk_tree_view_column_set_max_width(arg0, arg1);
}

int Pure_gtk_tree_view_column_get_max_width(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_max_width(arg0);
}

void Pure_gtk_tree_view_column_clicked(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_clicked(arg0);
}

void Pure_gtk_tree_view_column_set_title(GtkTreeViewColumn* arg0, char const* arg1)
{
  return gtk_tree_view_column_set_title(arg0, arg1);
}

char const* Pure_gtk_tree_view_column_get_title(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_title(arg0);
}

void Pure_gtk_tree_view_column_set_expand(GtkTreeViewColumn* arg0, int arg1)
{
  return gtk_tree_view_column_set_expand(arg0, arg1);
}

int Pure_gtk_tree_view_column_get_expand(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_expand(arg0);
}

void Pure_gtk_tree_view_column_set_clickable(GtkTreeViewColumn* arg0, int arg1)
{
  return gtk_tree_view_column_set_clickable(arg0, arg1);
}

int Pure_gtk_tree_view_column_get_clickable(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_clickable(arg0);
}

void Pure_gtk_tree_view_column_set_widget(GtkTreeViewColumn* arg0, GtkWidget* arg1)
{
  return gtk_tree_view_column_set_widget(arg0, arg1);
}

GtkWidget* Pure_gtk_tree_view_column_get_widget(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_widget(arg0);
}

void Pure_gtk_tree_view_column_set_alignment(GtkTreeViewColumn* arg0, float arg1)
{
  return gtk_tree_view_column_set_alignment(arg0, arg1);
}

float Pure_gtk_tree_view_column_get_alignment(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_alignment(arg0);
}

void Pure_gtk_tree_view_column_set_reorderable(GtkTreeViewColumn* arg0, int arg1)
{
  return gtk_tree_view_column_set_reorderable(arg0, arg1);
}

int Pure_gtk_tree_view_column_get_reorderable(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_reorderable(arg0);
}

void Pure_gtk_tree_view_column_set_sort_column_id(GtkTreeViewColumn* arg0, int arg1)
{
  return gtk_tree_view_column_set_sort_column_id(arg0, arg1);
}

int Pure_gtk_tree_view_column_get_sort_column_id(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_sort_column_id(arg0);
}

void Pure_gtk_tree_view_column_set_sort_indicator(GtkTreeViewColumn* arg0, int arg1)
{
  return gtk_tree_view_column_set_sort_indicator(arg0, arg1);
}

int Pure_gtk_tree_view_column_get_sort_indicator(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_sort_indicator(arg0);
}

void Pure_gtk_tree_view_column_set_sort_order(GtkTreeViewColumn* arg0, unsigned int arg1)
{
  return gtk_tree_view_column_set_sort_order(arg0, arg1);
}

unsigned int Pure_gtk_tree_view_column_get_sort_order(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_sort_order(arg0);
}

void Pure_gtk_tree_view_column_cell_set_cell_data(GtkTreeViewColumn* arg0, GtkTreeModel* arg1, GtkTreeIter* arg2, int arg3, int arg4)
{
  return gtk_tree_view_column_cell_set_cell_data(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_tree_view_column_cell_get_size(GtkTreeViewColumn* arg0, GdkRectangle const* arg1, int* arg2, int* arg3, int* arg4, int* arg5)
{
  return gtk_tree_view_column_cell_get_size(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gtk_tree_view_column_cell_is_visible(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_cell_is_visible(arg0);
}

void Pure_gtk_tree_view_column_focus_cell(GtkTreeViewColumn* arg0, GtkCellRenderer* arg1)
{
  return gtk_tree_view_column_focus_cell(arg0, arg1);
}

int Pure_gtk_tree_view_column_cell_get_position(GtkTreeViewColumn* arg0, GtkCellRenderer* arg1, int* arg2, int* arg3)
{
  return gtk_tree_view_column_cell_get_position(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_view_column_queue_resize(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_queue_resize(arg0);
}

GtkWidget* Pure_gtk_tree_view_column_get_tree_view(GtkTreeViewColumn* arg0)
{
  return gtk_tree_view_column_get_tree_view(arg0);
}

unsigned long Pure_gtk_cell_layout_get_type()
{
  return gtk_cell_layout_get_type();
}

void Pure_gtk_cell_layout_pack_start(GtkCellLayout* arg0, GtkCellRenderer* arg1, int arg2)
{
  return gtk_cell_layout_pack_start(arg0, arg1, arg2);
}

void Pure_gtk_cell_layout_pack_end(GtkCellLayout* arg0, GtkCellRenderer* arg1, int arg2)
{
  return gtk_cell_layout_pack_end(arg0, arg1, arg2);
}

GList* Pure_gtk_cell_layout_get_cells(GtkCellLayout* arg0)
{
  return gtk_cell_layout_get_cells(arg0);
}

void Pure_gtk_cell_layout_clear(GtkCellLayout* arg0)
{
  return gtk_cell_layout_clear(arg0);
}

void Pure_gtk_cell_layout_set_attributes(GtkCellLayout* arg0, GtkCellRenderer* arg1)
{
  return gtk_cell_layout_set_attributes(arg0, arg1);
}

void Pure_gtk_cell_layout_add_attribute(GtkCellLayout* arg0, GtkCellRenderer* arg1, char const* arg2, int arg3)
{
  return gtk_cell_layout_add_attribute(arg0, arg1, arg2, arg3);
}

void Pure_gtk_cell_layout_set_cell_data_func(GtkCellLayout* arg0, GtkCellRenderer* arg1, void* arg2, void* arg3, void* arg4)
{
  return gtk_cell_layout_set_cell_data_func(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_cell_layout_clear_attributes(GtkCellLayout* arg0, GtkCellRenderer* arg1)
{
  return gtk_cell_layout_clear_attributes(arg0, arg1);
}

void Pure_gtk_cell_layout_reorder(GtkCellLayout* arg0, GtkCellRenderer* arg1, int arg2)
{
  return gtk_cell_layout_reorder(arg0, arg1, arg2);
}

unsigned long Pure_gtk_cell_renderer_text_get_type()
{
  return gtk_cell_renderer_text_get_type();
}

GtkCellRenderer* Pure_gtk_cell_renderer_text_new()
{
  return gtk_cell_renderer_text_new();
}

void Pure_gtk_cell_renderer_text_set_fixed_height_from_font(GtkCellRendererText* arg0, int arg1)
{
  return gtk_cell_renderer_text_set_fixed_height_from_font(arg0, arg1);
}

unsigned long Pure_gtk_cell_renderer_accel_get_type()
{
  return gtk_cell_renderer_accel_get_type();
}

GtkCellRenderer* Pure_gtk_cell_renderer_accel_new()
{
  return gtk_cell_renderer_accel_new();
}

unsigned long Pure_gtk_cell_renderer_combo_get_type()
{
  return gtk_cell_renderer_combo_get_type();
}

GtkCellRenderer* Pure_gtk_cell_renderer_combo_new()
{
  return gtk_cell_renderer_combo_new();
}

unsigned long Pure_gtk_cell_renderer_pixbuf_get_type()
{
  return gtk_cell_renderer_pixbuf_get_type();
}

GtkCellRenderer* Pure_gtk_cell_renderer_pixbuf_new()
{
  return gtk_cell_renderer_pixbuf_new();
}

unsigned long Pure_gtk_cell_renderer_progress_get_type()
{
  return gtk_cell_renderer_progress_get_type();
}

GtkCellRenderer* Pure_gtk_cell_renderer_progress_new()
{
  return gtk_cell_renderer_progress_new();
}

unsigned long Pure_gtk_cell_renderer_spin_get_type()
{
  return gtk_cell_renderer_spin_get_type();
}

GtkCellRenderer* Pure_gtk_cell_renderer_spin_new()
{
  return gtk_cell_renderer_spin_new();
}

unsigned long Pure_gtk_cell_renderer_toggle_get_type()
{
  return gtk_cell_renderer_toggle_get_type();
}

GtkCellRenderer* Pure_gtk_cell_renderer_toggle_new()
{
  return gtk_cell_renderer_toggle_new();
}

int Pure_gtk_cell_renderer_toggle_get_radio(GtkCellRendererToggle* arg0)
{
  return gtk_cell_renderer_toggle_get_radio(arg0);
}

void Pure_gtk_cell_renderer_toggle_set_radio(GtkCellRendererToggle* arg0, int arg1)
{
  return gtk_cell_renderer_toggle_set_radio(arg0, arg1);
}

int Pure_gtk_cell_renderer_toggle_get_active(GtkCellRendererToggle* arg0)
{
  return gtk_cell_renderer_toggle_get_active(arg0);
}

void Pure_gtk_cell_renderer_toggle_set_active(GtkCellRendererToggle* arg0, int arg1)
{
  return gtk_cell_renderer_toggle_set_active(arg0, arg1);
}

unsigned long Pure_gtk_cell_view_get_type()
{
  return gtk_cell_view_get_type();
}

GtkWidget* Pure_gtk_cell_view_new()
{
  return gtk_cell_view_new();
}

GtkWidget* Pure_gtk_cell_view_new_with_text(char const* arg0)
{
  return gtk_cell_view_new_with_text(arg0);
}

GtkWidget* Pure_gtk_cell_view_new_with_markup(char const* arg0)
{
  return gtk_cell_view_new_with_markup(arg0);
}

GtkWidget* Pure_gtk_cell_view_new_with_pixbuf(GdkPixbuf* arg0)
{
  return gtk_cell_view_new_with_pixbuf(arg0);
}

void Pure_gtk_cell_view_set_model(GtkCellView* arg0, GtkTreeModel* arg1)
{
  return gtk_cell_view_set_model(arg0, arg1);
}

void Pure_gtk_cell_view_set_displayed_row(GtkCellView* arg0, GtkTreePath* arg1)
{
  return gtk_cell_view_set_displayed_row(arg0, arg1);
}

GtkTreePath* Pure_gtk_cell_view_get_displayed_row(GtkCellView* arg0)
{
  return gtk_cell_view_get_displayed_row(arg0);
}

int Pure_gtk_cell_view_get_size_of_row(GtkCellView* arg0, GtkTreePath* arg1, GtkRequisition* arg2)
{
  return gtk_cell_view_get_size_of_row(arg0, arg1, arg2);
}

void Pure_gtk_cell_view_set_background_color(GtkCellView* arg0, GdkColor const* arg1)
{
  return gtk_cell_view_set_background_color(arg0, arg1);
}

GList* Pure_gtk_cell_view_get_cell_renderers(GtkCellView* arg0)
{
  return gtk_cell_view_get_cell_renderers(arg0);
}

unsigned long Pure_gtk_toggle_button_get_type()
{
  return gtk_toggle_button_get_type();
}

GtkWidget* Pure_gtk_toggle_button_new()
{
  return gtk_toggle_button_new();
}

GtkWidget* Pure_gtk_toggle_button_new_with_label(char const* arg0)
{
  return gtk_toggle_button_new_with_label(arg0);
}

GtkWidget* Pure_gtk_toggle_button_new_with_mnemonic(char const* arg0)
{
  return gtk_toggle_button_new_with_mnemonic(arg0);
}

void Pure_gtk_toggle_button_set_mode(GtkToggleButton* arg0, int arg1)
{
  return gtk_toggle_button_set_mode(arg0, arg1);
}

int Pure_gtk_toggle_button_get_mode(GtkToggleButton* arg0)
{
  return gtk_toggle_button_get_mode(arg0);
}

void Pure_gtk_toggle_button_set_active(GtkToggleButton* arg0, int arg1)
{
  return gtk_toggle_button_set_active(arg0, arg1);
}

int Pure_gtk_toggle_button_get_active(GtkToggleButton* arg0)
{
  return gtk_toggle_button_get_active(arg0);
}

void Pure_gtk_toggle_button_toggled(GtkToggleButton* arg0)
{
  return gtk_toggle_button_toggled(arg0);
}

void Pure_gtk_toggle_button_set_inconsistent(GtkToggleButton* arg0, int arg1)
{
  return gtk_toggle_button_set_inconsistent(arg0, arg1);
}

int Pure_gtk_toggle_button_get_inconsistent(GtkToggleButton* arg0)
{
  return gtk_toggle_button_get_inconsistent(arg0);
}

unsigned long Pure_gtk_check_button_get_type()
{
  return gtk_check_button_get_type();
}

GtkWidget* Pure_gtk_check_button_new()
{
  return gtk_check_button_new();
}

GtkWidget* Pure_gtk_check_button_new_with_label(char const* arg0)
{
  return gtk_check_button_new_with_label(arg0);
}

GtkWidget* Pure_gtk_check_button_new_with_mnemonic(char const* arg0)
{
  return gtk_check_button_new_with_mnemonic(arg0);
}

unsigned long Pure_gtk_item_get_type()
{
  return gtk_item_get_type();
}

void Pure_gtk_item_select(GtkItem* arg0)
{
  return gtk_item_select(arg0);
}

void Pure_gtk_item_deselect(GtkItem* arg0)
{
  return gtk_item_deselect(arg0);
}

void Pure_gtk_item_toggle(GtkItem* arg0)
{
  return gtk_item_toggle(arg0);
}

unsigned long Pure_gtk_menu_item_get_type()
{
  return gtk_menu_item_get_type();
}

GtkWidget* Pure_gtk_menu_item_new()
{
  return gtk_menu_item_new();
}

GtkWidget* Pure_gtk_menu_item_new_with_label(char const* arg0)
{
  return gtk_menu_item_new_with_label(arg0);
}

GtkWidget* Pure_gtk_menu_item_new_with_mnemonic(char const* arg0)
{
  return gtk_menu_item_new_with_mnemonic(arg0);
}

void Pure_gtk_menu_item_set_submenu(GtkMenuItem* arg0, GtkWidget* arg1)
{
  return gtk_menu_item_set_submenu(arg0, arg1);
}

GtkWidget* Pure_gtk_menu_item_get_submenu(GtkMenuItem* arg0)
{
  return gtk_menu_item_get_submenu(arg0);
}

void Pure_gtk_menu_item_select(GtkMenuItem* arg0)
{
  return gtk_menu_item_select(arg0);
}

void Pure_gtk_menu_item_deselect(GtkMenuItem* arg0)
{
  return gtk_menu_item_deselect(arg0);
}

void Pure_gtk_menu_item_activate(GtkMenuItem* arg0)
{
  return gtk_menu_item_activate(arg0);
}

void Pure_gtk_menu_item_toggle_size_request(GtkMenuItem* arg0, int* arg1)
{
  return gtk_menu_item_toggle_size_request(arg0, arg1);
}

void Pure_gtk_menu_item_toggle_size_allocate(GtkMenuItem* arg0, int arg1)
{
  return gtk_menu_item_toggle_size_allocate(arg0, arg1);
}

void Pure_gtk_menu_item_set_right_justified(GtkMenuItem* arg0, int arg1)
{
  return gtk_menu_item_set_right_justified(arg0, arg1);
}

int Pure_gtk_menu_item_get_right_justified(GtkMenuItem* arg0)
{
  return gtk_menu_item_get_right_justified(arg0);
}

void Pure_gtk_menu_item_set_accel_path(GtkMenuItem* arg0, char const* arg1)
{
  return gtk_menu_item_set_accel_path(arg0, arg1);
}

char const* Pure_gtk_menu_item_get_accel_path(GtkMenuItem* arg0)
{
  return gtk_menu_item_get_accel_path(arg0);
}

void Pure_gtk_menu_item_remove_submenu(GtkMenuItem* arg0)
{
  return gtk_menu_item_remove_submenu(arg0);
}

unsigned long Pure_gtk_check_menu_item_get_type()
{
  return gtk_check_menu_item_get_type();
}

GtkWidget* Pure_gtk_check_menu_item_new()
{
  return gtk_check_menu_item_new();
}

GtkWidget* Pure_gtk_check_menu_item_new_with_label(char const* arg0)
{
  return gtk_check_menu_item_new_with_label(arg0);
}

GtkWidget* Pure_gtk_check_menu_item_new_with_mnemonic(char const* arg0)
{
  return gtk_check_menu_item_new_with_mnemonic(arg0);
}

void Pure_gtk_check_menu_item_set_active(GtkCheckMenuItem* arg0, int arg1)
{
  return gtk_check_menu_item_set_active(arg0, arg1);
}

int Pure_gtk_check_menu_item_get_active(GtkCheckMenuItem* arg0)
{
  return gtk_check_menu_item_get_active(arg0);
}

void Pure_gtk_check_menu_item_toggled(GtkCheckMenuItem* arg0)
{
  return gtk_check_menu_item_toggled(arg0);
}

void Pure_gtk_check_menu_item_set_inconsistent(GtkCheckMenuItem* arg0, int arg1)
{
  return gtk_check_menu_item_set_inconsistent(arg0, arg1);
}

int Pure_gtk_check_menu_item_get_inconsistent(GtkCheckMenuItem* arg0)
{
  return gtk_check_menu_item_get_inconsistent(arg0);
}

void Pure_gtk_check_menu_item_set_draw_as_radio(GtkCheckMenuItem* arg0, int arg1)
{
  return gtk_check_menu_item_set_draw_as_radio(arg0, arg1);
}

int Pure_gtk_check_menu_item_get_draw_as_radio(GtkCheckMenuItem* arg0)
{
  return gtk_check_menu_item_get_draw_as_radio(arg0);
}

void Pure_gtk_check_menu_item_set_show_toggle(GtkCheckMenuItem* arg0, int arg1)
{
  return gtk_check_menu_item_set_show_toggle(arg0, arg1);
}

unsigned long Pure_gtk_text_tag_get_type()
{
  return gtk_text_tag_get_type();
}

GtkTextTag* Pure_gtk_text_tag_new(char const* arg0)
{
  return gtk_text_tag_new(arg0);
}

int Pure_gtk_text_tag_get_priority(GtkTextTag* arg0)
{
  return gtk_text_tag_get_priority(arg0);
}

void Pure_gtk_text_tag_set_priority(GtkTextTag* arg0, int arg1)
{
  return gtk_text_tag_set_priority(arg0, arg1);
}

int Pure_gtk_text_tag_event(GtkTextTag* arg0, GObject* arg1, GdkEvent* arg2, GtkTextIter const* arg3)
{
  return gtk_text_tag_event(arg0, arg1, arg2, arg3);
}

GtkTextAttributes* Pure_gtk_text_attributes_new()
{
  return gtk_text_attributes_new();
}

GtkTextAttributes* Pure_gtk_text_attributes_copy(GtkTextAttributes* arg0)
{
  return gtk_text_attributes_copy(arg0);
}

void Pure_gtk_text_attributes_copy_values(GtkTextAttributes* arg0, GtkTextAttributes* arg1)
{
  return gtk_text_attributes_copy_values(arg0, arg1);
}

void Pure_gtk_text_attributes_unref(GtkTextAttributes* arg0)
{
  return gtk_text_attributes_unref(arg0);
}

GtkTextAttributes* Pure_gtk_text_attributes_ref(GtkTextAttributes* arg0)
{
  return gtk_text_attributes_ref(arg0);
}

unsigned long Pure_gtk_text_attributes_get_type()
{
  return gtk_text_attributes_get_type();
}

unsigned long Pure_gtk_text_child_anchor_get_type()
{
  return gtk_text_child_anchor_get_type();
}

GtkTextChildAnchor* Pure_gtk_text_child_anchor_new()
{
  return gtk_text_child_anchor_new();
}

GList* Pure_gtk_text_child_anchor_get_widgets(GtkTextChildAnchor* arg0)
{
  return gtk_text_child_anchor_get_widgets(arg0);
}

int Pure_gtk_text_child_anchor_get_deleted(GtkTextChildAnchor* arg0)
{
  return gtk_text_child_anchor_get_deleted(arg0);
}

GtkTextBuffer* Pure_gtk_text_iter_get_buffer(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_buffer(arg0);
}

GtkTextIter* Pure_gtk_text_iter_copy(GtkTextIter const* arg0)
{
  return gtk_text_iter_copy(arg0);
}

void Pure_gtk_text_iter_free(GtkTextIter* arg0)
{
  return gtk_text_iter_free(arg0);
}

unsigned long Pure_gtk_text_iter_get_type()
{
  return gtk_text_iter_get_type();
}

int Pure_gtk_text_iter_get_offset(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_offset(arg0);
}

int Pure_gtk_text_iter_get_line(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_line(arg0);
}

int Pure_gtk_text_iter_get_line_offset(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_line_offset(arg0);
}

int Pure_gtk_text_iter_get_line_index(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_line_index(arg0);
}

int Pure_gtk_text_iter_get_visible_line_offset(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_visible_line_offset(arg0);
}

int Pure_gtk_text_iter_get_visible_line_index(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_visible_line_index(arg0);
}

unsigned int Pure_gtk_text_iter_get_char(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_char(arg0);
}

char* Pure_gtk_text_iter_get_slice(GtkTextIter const* arg0, GtkTextIter const* arg1)
{
  return gtk_text_iter_get_slice(arg0, arg1);
}

char* Pure_gtk_text_iter_get_text(GtkTextIter const* arg0, GtkTextIter const* arg1)
{
  return gtk_text_iter_get_text(arg0, arg1);
}

char* Pure_gtk_text_iter_get_visible_slice(GtkTextIter const* arg0, GtkTextIter const* arg1)
{
  return gtk_text_iter_get_visible_slice(arg0, arg1);
}

char* Pure_gtk_text_iter_get_visible_text(GtkTextIter const* arg0, GtkTextIter const* arg1)
{
  return gtk_text_iter_get_visible_text(arg0, arg1);
}

GdkPixbuf* Pure_gtk_text_iter_get_pixbuf(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_pixbuf(arg0);
}

GSList* Pure_gtk_text_iter_get_marks(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_marks(arg0);
}

GtkTextChildAnchor* Pure_gtk_text_iter_get_child_anchor(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_child_anchor(arg0);
}

GSList* Pure_gtk_text_iter_get_toggled_tags(GtkTextIter const* arg0, int arg1)
{
  return gtk_text_iter_get_toggled_tags(arg0, arg1);
}

int Pure_gtk_text_iter_begins_tag(GtkTextIter const* arg0, GtkTextTag* arg1)
{
  return gtk_text_iter_begins_tag(arg0, arg1);
}

int Pure_gtk_text_iter_ends_tag(GtkTextIter const* arg0, GtkTextTag* arg1)
{
  return gtk_text_iter_ends_tag(arg0, arg1);
}

int Pure_gtk_text_iter_toggles_tag(GtkTextIter const* arg0, GtkTextTag* arg1)
{
  return gtk_text_iter_toggles_tag(arg0, arg1);
}

int Pure_gtk_text_iter_has_tag(GtkTextIter const* arg0, GtkTextTag* arg1)
{
  return gtk_text_iter_has_tag(arg0, arg1);
}

GSList* Pure_gtk_text_iter_get_tags(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_tags(arg0);
}

int Pure_gtk_text_iter_editable(GtkTextIter const* arg0, int arg1)
{
  return gtk_text_iter_editable(arg0, arg1);
}

int Pure_gtk_text_iter_can_insert(GtkTextIter const* arg0, int arg1)
{
  return gtk_text_iter_can_insert(arg0, arg1);
}

int Pure_gtk_text_iter_starts_word(GtkTextIter const* arg0)
{
  return gtk_text_iter_starts_word(arg0);
}

int Pure_gtk_text_iter_ends_word(GtkTextIter const* arg0)
{
  return gtk_text_iter_ends_word(arg0);
}

int Pure_gtk_text_iter_inside_word(GtkTextIter const* arg0)
{
  return gtk_text_iter_inside_word(arg0);
}

int Pure_gtk_text_iter_starts_sentence(GtkTextIter const* arg0)
{
  return gtk_text_iter_starts_sentence(arg0);
}

int Pure_gtk_text_iter_ends_sentence(GtkTextIter const* arg0)
{
  return gtk_text_iter_ends_sentence(arg0);
}

int Pure_gtk_text_iter_inside_sentence(GtkTextIter const* arg0)
{
  return gtk_text_iter_inside_sentence(arg0);
}

int Pure_gtk_text_iter_starts_line(GtkTextIter const* arg0)
{
  return gtk_text_iter_starts_line(arg0);
}

int Pure_gtk_text_iter_ends_line(GtkTextIter const* arg0)
{
  return gtk_text_iter_ends_line(arg0);
}

int Pure_gtk_text_iter_is_cursor_position(GtkTextIter const* arg0)
{
  return gtk_text_iter_is_cursor_position(arg0);
}

int Pure_gtk_text_iter_get_chars_in_line(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_chars_in_line(arg0);
}

int Pure_gtk_text_iter_get_bytes_in_line(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_bytes_in_line(arg0);
}

int Pure_gtk_text_iter_get_attributes(GtkTextIter const* arg0, GtkTextAttributes* arg1)
{
  return gtk_text_iter_get_attributes(arg0, arg1);
}

PangoLanguage* Pure_gtk_text_iter_get_language(GtkTextIter const* arg0)
{
  return gtk_text_iter_get_language(arg0);
}

int Pure_gtk_text_iter_is_end(GtkTextIter const* arg0)
{
  return gtk_text_iter_is_end(arg0);
}

int Pure_gtk_text_iter_is_start(GtkTextIter const* arg0)
{
  return gtk_text_iter_is_start(arg0);
}

int Pure_gtk_text_iter_forward_char(GtkTextIter* arg0)
{
  return gtk_text_iter_forward_char(arg0);
}

int Pure_gtk_text_iter_backward_char(GtkTextIter* arg0)
{
  return gtk_text_iter_backward_char(arg0);
}

int Pure_gtk_text_iter_forward_chars(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_forward_chars(arg0, arg1);
}

int Pure_gtk_text_iter_backward_chars(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_backward_chars(arg0, arg1);
}

int Pure_gtk_text_iter_forward_line(GtkTextIter* arg0)
{
  return gtk_text_iter_forward_line(arg0);
}

int Pure_gtk_text_iter_backward_line(GtkTextIter* arg0)
{
  return gtk_text_iter_backward_line(arg0);
}

int Pure_gtk_text_iter_forward_lines(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_forward_lines(arg0, arg1);
}

int Pure_gtk_text_iter_backward_lines(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_backward_lines(arg0, arg1);
}

int Pure_gtk_text_iter_forward_word_end(GtkTextIter* arg0)
{
  return gtk_text_iter_forward_word_end(arg0);
}

int Pure_gtk_text_iter_backward_word_start(GtkTextIter* arg0)
{
  return gtk_text_iter_backward_word_start(arg0);
}

int Pure_gtk_text_iter_forward_word_ends(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_forward_word_ends(arg0, arg1);
}

int Pure_gtk_text_iter_backward_word_starts(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_backward_word_starts(arg0, arg1);
}

int Pure_gtk_text_iter_forward_visible_line(GtkTextIter* arg0)
{
  return gtk_text_iter_forward_visible_line(arg0);
}

int Pure_gtk_text_iter_backward_visible_line(GtkTextIter* arg0)
{
  return gtk_text_iter_backward_visible_line(arg0);
}

int Pure_gtk_text_iter_forward_visible_lines(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_forward_visible_lines(arg0, arg1);
}

int Pure_gtk_text_iter_backward_visible_lines(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_backward_visible_lines(arg0, arg1);
}

int Pure_gtk_text_iter_forward_visible_word_end(GtkTextIter* arg0)
{
  return gtk_text_iter_forward_visible_word_end(arg0);
}

int Pure_gtk_text_iter_backward_visible_word_start(GtkTextIter* arg0)
{
  return gtk_text_iter_backward_visible_word_start(arg0);
}

int Pure_gtk_text_iter_forward_visible_word_ends(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_forward_visible_word_ends(arg0, arg1);
}

int Pure_gtk_text_iter_backward_visible_word_starts(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_backward_visible_word_starts(arg0, arg1);
}

int Pure_gtk_text_iter_forward_sentence_end(GtkTextIter* arg0)
{
  return gtk_text_iter_forward_sentence_end(arg0);
}

int Pure_gtk_text_iter_backward_sentence_start(GtkTextIter* arg0)
{
  return gtk_text_iter_backward_sentence_start(arg0);
}

int Pure_gtk_text_iter_forward_sentence_ends(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_forward_sentence_ends(arg0, arg1);
}

int Pure_gtk_text_iter_backward_sentence_starts(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_backward_sentence_starts(arg0, arg1);
}

int Pure_gtk_text_iter_forward_cursor_position(GtkTextIter* arg0)
{
  return gtk_text_iter_forward_cursor_position(arg0);
}

int Pure_gtk_text_iter_backward_cursor_position(GtkTextIter* arg0)
{
  return gtk_text_iter_backward_cursor_position(arg0);
}

int Pure_gtk_text_iter_forward_cursor_positions(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_forward_cursor_positions(arg0, arg1);
}

int Pure_gtk_text_iter_backward_cursor_positions(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_backward_cursor_positions(arg0, arg1);
}

int Pure_gtk_text_iter_forward_visible_cursor_position(GtkTextIter* arg0)
{
  return gtk_text_iter_forward_visible_cursor_position(arg0);
}

int Pure_gtk_text_iter_backward_visible_cursor_position(GtkTextIter* arg0)
{
  return gtk_text_iter_backward_visible_cursor_position(arg0);
}

int Pure_gtk_text_iter_forward_visible_cursor_positions(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_forward_visible_cursor_positions(arg0, arg1);
}

int Pure_gtk_text_iter_backward_visible_cursor_positions(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_backward_visible_cursor_positions(arg0, arg1);
}

void Pure_gtk_text_iter_set_offset(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_set_offset(arg0, arg1);
}

void Pure_gtk_text_iter_set_line(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_set_line(arg0, arg1);
}

void Pure_gtk_text_iter_set_line_offset(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_set_line_offset(arg0, arg1);
}

void Pure_gtk_text_iter_set_line_index(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_set_line_index(arg0, arg1);
}

void Pure_gtk_text_iter_forward_to_end(GtkTextIter* arg0)
{
  return gtk_text_iter_forward_to_end(arg0);
}

int Pure_gtk_text_iter_forward_to_line_end(GtkTextIter* arg0)
{
  return gtk_text_iter_forward_to_line_end(arg0);
}

void Pure_gtk_text_iter_set_visible_line_offset(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_set_visible_line_offset(arg0, arg1);
}

void Pure_gtk_text_iter_set_visible_line_index(GtkTextIter* arg0, int arg1)
{
  return gtk_text_iter_set_visible_line_index(arg0, arg1);
}

int Pure_gtk_text_iter_forward_to_tag_toggle(GtkTextIter* arg0, GtkTextTag* arg1)
{
  return gtk_text_iter_forward_to_tag_toggle(arg0, arg1);
}

int Pure_gtk_text_iter_backward_to_tag_toggle(GtkTextIter* arg0, GtkTextTag* arg1)
{
  return gtk_text_iter_backward_to_tag_toggle(arg0, arg1);
}

int Pure_gtk_text_iter_forward_find_char(GtkTextIter* arg0, void* arg1, void* arg2, GtkTextIter const* arg3)
{
  return gtk_text_iter_forward_find_char(arg0, arg1, arg2, arg3);
}

int Pure_gtk_text_iter_backward_find_char(GtkTextIter* arg0, void* arg1, void* arg2, GtkTextIter const* arg3)
{
  return gtk_text_iter_backward_find_char(arg0, arg1, arg2, arg3);
}

int Pure_gtk_text_iter_forward_search(GtkTextIter const* arg0, char const* arg1, unsigned int arg2, GtkTextIter* arg3, GtkTextIter* arg4, GtkTextIter const* arg5)
{
  return gtk_text_iter_forward_search(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gtk_text_iter_backward_search(GtkTextIter const* arg0, char const* arg1, unsigned int arg2, GtkTextIter* arg3, GtkTextIter* arg4, GtkTextIter const* arg5)
{
  return gtk_text_iter_backward_search(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gtk_text_iter_equal(GtkTextIter const* arg0, GtkTextIter const* arg1)
{
  return gtk_text_iter_equal(arg0, arg1);
}

int Pure_gtk_text_iter_compare(GtkTextIter const* arg0, GtkTextIter const* arg1)
{
  return gtk_text_iter_compare(arg0, arg1);
}

int Pure_gtk_text_iter_in_range(GtkTextIter const* arg0, GtkTextIter const* arg1, GtkTextIter const* arg2)
{
  return gtk_text_iter_in_range(arg0, arg1, arg2);
}

void Pure_gtk_text_iter_order(GtkTextIter* arg0, GtkTextIter* arg1)
{
  return gtk_text_iter_order(arg0, arg1);
}

GtkTargetList* Pure_gtk_target_list_new(GtkTargetEntry const* arg0, unsigned int arg1)
{
  return gtk_target_list_new(arg0, arg1);
}

GtkTargetList* Pure_gtk_target_list_ref(GtkTargetList* arg0)
{
  return gtk_target_list_ref(arg0);
}

void Pure_gtk_target_list_unref(GtkTargetList* arg0)
{
  return gtk_target_list_unref(arg0);
}

void Pure_gtk_target_list_add(GtkTargetList* arg0, struct _GdkAtom* arg1, unsigned int arg2, unsigned int arg3)
{
  return gtk_target_list_add(arg0, arg1, arg2, arg3);
}

void Pure_gtk_target_list_add_text_targets(GtkTargetList* arg0, unsigned int arg1)
{
  return gtk_target_list_add_text_targets(arg0, arg1);
}

void Pure_gtk_target_list_add_rich_text_targets(GtkTargetList* arg0, unsigned int arg1, int arg2, GtkTextBuffer* arg3)
{
  return gtk_target_list_add_rich_text_targets(arg0, arg1, arg2, arg3);
}

void Pure_gtk_target_list_add_image_targets(GtkTargetList* arg0, unsigned int arg1, int arg2)
{
  return gtk_target_list_add_image_targets(arg0, arg1, arg2);
}

void Pure_gtk_target_list_add_uri_targets(GtkTargetList* arg0, unsigned int arg1)
{
  return gtk_target_list_add_uri_targets(arg0, arg1);
}

void Pure_gtk_target_list_add_table(GtkTargetList* arg0, GtkTargetEntry const* arg1, unsigned int arg2)
{
  return gtk_target_list_add_table(arg0, arg1, arg2);
}

void Pure_gtk_target_list_remove(GtkTargetList* arg0, struct _GdkAtom* arg1)
{
  return gtk_target_list_remove(arg0, arg1);
}

int Pure_gtk_target_list_find(GtkTargetList* arg0, struct _GdkAtom* arg1, unsigned int* arg2)
{
  return gtk_target_list_find(arg0, arg1, arg2);
}

GtkTargetEntry* Pure_gtk_target_table_new_from_list(GtkTargetList* arg0, int* arg1)
{
  return gtk_target_table_new_from_list(arg0, arg1);
}

void Pure_gtk_target_table_free(GtkTargetEntry* arg0, int arg1)
{
  return gtk_target_table_free(arg0, arg1);
}

int Pure_gtk_selection_owner_set(GtkWidget* arg0, struct _GdkAtom* arg1, unsigned int arg2)
{
  return gtk_selection_owner_set(arg0, arg1, arg2);
}

int Pure_gtk_selection_owner_set_for_display(GdkDisplay* arg0, GtkWidget* arg1, struct _GdkAtom* arg2, unsigned int arg3)
{
  return gtk_selection_owner_set_for_display(arg0, arg1, arg2, arg3);
}

void Pure_gtk_selection_add_target(GtkWidget* arg0, struct _GdkAtom* arg1, struct _GdkAtom* arg2, unsigned int arg3)
{
  return gtk_selection_add_target(arg0, arg1, arg2, arg3);
}

void Pure_gtk_selection_add_targets(GtkWidget* arg0, struct _GdkAtom* arg1, GtkTargetEntry const* arg2, unsigned int arg3)
{
  return gtk_selection_add_targets(arg0, arg1, arg2, arg3);
}

void Pure_gtk_selection_clear_targets(GtkWidget* arg0, struct _GdkAtom* arg1)
{
  return gtk_selection_clear_targets(arg0, arg1);
}

int Pure_gtk_selection_convert(GtkWidget* arg0, struct _GdkAtom* arg1, struct _GdkAtom* arg2, unsigned int arg3)
{
  return gtk_selection_convert(arg0, arg1, arg2, arg3);
}

struct _GdkAtom* Pure_gtk_selection_data_get_target(GtkSelectionData* arg0)
{
  return gtk_selection_data_get_target(arg0);
}

struct _GdkAtom* Pure_gtk_selection_data_get_data_type(GtkSelectionData* arg0)
{
  return gtk_selection_data_get_data_type(arg0);
}

int Pure_gtk_selection_data_get_format(GtkSelectionData* arg0)
{
  return gtk_selection_data_get_format(arg0);
}

unsigned char const* Pure_gtk_selection_data_get_data(GtkSelectionData* arg0)
{
  return gtk_selection_data_get_data(arg0);
}

int Pure_gtk_selection_data_get_length(GtkSelectionData* arg0)
{
  return gtk_selection_data_get_length(arg0);
}

GdkDisplay* Pure_gtk_selection_data_get_display(GtkSelectionData* arg0)
{
  return gtk_selection_data_get_display(arg0);
}

void Pure_gtk_selection_data_set(GtkSelectionData* arg0, struct _GdkAtom* arg1, int arg2, unsigned char const* arg3, int arg4)
{
  return gtk_selection_data_set(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gtk_selection_data_set_text(GtkSelectionData* arg0, char const* arg1, int arg2)
{
  return gtk_selection_data_set_text(arg0, arg1, arg2);
}

unsigned char* Pure_gtk_selection_data_get_text(GtkSelectionData* arg0)
{
  return gtk_selection_data_get_text(arg0);
}

int Pure_gtk_selection_data_set_pixbuf(GtkSelectionData* arg0, GdkPixbuf* arg1)
{
  return gtk_selection_data_set_pixbuf(arg0, arg1);
}

GdkPixbuf* Pure_gtk_selection_data_get_pixbuf(GtkSelectionData* arg0)
{
  return gtk_selection_data_get_pixbuf(arg0);
}

int Pure_gtk_selection_data_set_uris(GtkSelectionData* arg0, char** arg1)
{
  return gtk_selection_data_set_uris(arg0, arg1);
}

char** Pure_gtk_selection_data_get_uris(GtkSelectionData* arg0)
{
  return gtk_selection_data_get_uris(arg0);
}

int Pure_gtk_selection_data_get_targets(GtkSelectionData* arg0, struct _GdkAtom*** arg1, int* arg2)
{
  return gtk_selection_data_get_targets(arg0, arg1, arg2);
}

int Pure_gtk_selection_data_targets_include_text(GtkSelectionData* arg0)
{
  return gtk_selection_data_targets_include_text(arg0);
}

int Pure_gtk_selection_data_targets_include_rich_text(GtkSelectionData* arg0, GtkTextBuffer* arg1)
{
  return gtk_selection_data_targets_include_rich_text(arg0, arg1);
}

int Pure_gtk_selection_data_targets_include_image(GtkSelectionData* arg0, int arg1)
{
  return gtk_selection_data_targets_include_image(arg0, arg1);
}

int Pure_gtk_selection_data_targets_include_uri(GtkSelectionData* arg0)
{
  return gtk_selection_data_targets_include_uri(arg0);
}

int Pure_gtk_targets_include_text(struct _GdkAtom** arg0, int arg1)
{
  return gtk_targets_include_text(arg0, arg1);
}

int Pure_gtk_targets_include_rich_text(struct _GdkAtom** arg0, int arg1, GtkTextBuffer* arg2)
{
  return gtk_targets_include_rich_text(arg0, arg1, arg2);
}

int Pure_gtk_targets_include_image(struct _GdkAtom** arg0, int arg1, int arg2)
{
  return gtk_targets_include_image(arg0, arg1, arg2);
}

int Pure_gtk_targets_include_uri(struct _GdkAtom** arg0, int arg1)
{
  return gtk_targets_include_uri(arg0, arg1);
}

void Pure_gtk_selection_remove_all(GtkWidget* arg0)
{
  return gtk_selection_remove_all(arg0);
}

int Pure_gtk_selection_clear(GtkWidget* arg0, GdkEventSelection* arg1)
{
  return gtk_selection_clear(arg0, arg1);
}

unsigned long Pure_gtk_selection_data_get_type()
{
  return gtk_selection_data_get_type();
}

GtkSelectionData* Pure_gtk_selection_data_copy(GtkSelectionData* arg0)
{
  return gtk_selection_data_copy(arg0);
}

void Pure_gtk_selection_data_free(GtkSelectionData* arg0)
{
  return gtk_selection_data_free(arg0);
}

unsigned long Pure_gtk_target_list_get_type()
{
  return gtk_target_list_get_type();
}

unsigned long Pure_gtk_clipboard_get_type()
{
  return gtk_clipboard_get_type();
}

GtkClipboard* Pure_gtk_clipboard_get_for_display(GdkDisplay* arg0, struct _GdkAtom* arg1)
{
  return gtk_clipboard_get_for_display(arg0, arg1);
}

GtkClipboard* Pure_gtk_clipboard_get(struct _GdkAtom* arg0)
{
  return gtk_clipboard_get(arg0);
}

GdkDisplay* Pure_gtk_clipboard_get_display(GtkClipboard* arg0)
{
  return gtk_clipboard_get_display(arg0);
}

int Pure_gtk_clipboard_set_with_data(GtkClipboard* arg0, GtkTargetEntry const* arg1, unsigned int arg2, void* arg3, void* arg4, void* arg5)
{
  return gtk_clipboard_set_with_data(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gtk_clipboard_set_with_owner(GtkClipboard* arg0, GtkTargetEntry const* arg1, unsigned int arg2, void* arg3, void* arg4, GObject* arg5)
{
  return gtk_clipboard_set_with_owner(arg0, arg1, arg2, arg3, arg4, arg5);
}

GObject* Pure_gtk_clipboard_get_owner(GtkClipboard* arg0)
{
  return gtk_clipboard_get_owner(arg0);
}

void Pure_gtk_clipboard_clear(GtkClipboard* arg0)
{
  return gtk_clipboard_clear(arg0);
}

void Pure_gtk_clipboard_set_text(GtkClipboard* arg0, char const* arg1, int arg2)
{
  return gtk_clipboard_set_text(arg0, arg1, arg2);
}

void Pure_gtk_clipboard_set_image(GtkClipboard* arg0, GdkPixbuf* arg1)
{
  return gtk_clipboard_set_image(arg0, arg1);
}

void Pure_gtk_clipboard_request_contents(GtkClipboard* arg0, struct _GdkAtom* arg1, void* arg2, void* arg3)
{
  return gtk_clipboard_request_contents(arg0, arg1, arg2, arg3);
}

void Pure_gtk_clipboard_request_text(GtkClipboard* arg0, void* arg1, void* arg2)
{
  return gtk_clipboard_request_text(arg0, arg1, arg2);
}

void Pure_gtk_clipboard_request_rich_text(GtkClipboard* arg0, GtkTextBuffer* arg1, void* arg2, void* arg3)
{
  return gtk_clipboard_request_rich_text(arg0, arg1, arg2, arg3);
}

void Pure_gtk_clipboard_request_image(GtkClipboard* arg0, void* arg1, void* arg2)
{
  return gtk_clipboard_request_image(arg0, arg1, arg2);
}

void Pure_gtk_clipboard_request_uris(GtkClipboard* arg0, void* arg1, void* arg2)
{
  return gtk_clipboard_request_uris(arg0, arg1, arg2);
}

void Pure_gtk_clipboard_request_targets(GtkClipboard* arg0, void* arg1, void* arg2)
{
  return gtk_clipboard_request_targets(arg0, arg1, arg2);
}

GtkSelectionData* Pure_gtk_clipboard_wait_for_contents(GtkClipboard* arg0, struct _GdkAtom* arg1)
{
  return gtk_clipboard_wait_for_contents(arg0, arg1);
}

char* Pure_gtk_clipboard_wait_for_text(GtkClipboard* arg0)
{
  return gtk_clipboard_wait_for_text(arg0);
}

unsigned char* Pure_gtk_clipboard_wait_for_rich_text(GtkClipboard* arg0, GtkTextBuffer* arg1, struct _GdkAtom** arg2, unsigned long* arg3)
{
  return gtk_clipboard_wait_for_rich_text(arg0, arg1, arg2, arg3);
}

GdkPixbuf* Pure_gtk_clipboard_wait_for_image(GtkClipboard* arg0)
{
  return gtk_clipboard_wait_for_image(arg0);
}

char** Pure_gtk_clipboard_wait_for_uris(GtkClipboard* arg0)
{
  return gtk_clipboard_wait_for_uris(arg0);
}

int Pure_gtk_clipboard_wait_for_targets(GtkClipboard* arg0, struct _GdkAtom*** arg1, int* arg2)
{
  return gtk_clipboard_wait_for_targets(arg0, arg1, arg2);
}

int Pure_gtk_clipboard_wait_is_text_available(GtkClipboard* arg0)
{
  return gtk_clipboard_wait_is_text_available(arg0);
}

int Pure_gtk_clipboard_wait_is_rich_text_available(GtkClipboard* arg0, GtkTextBuffer* arg1)
{
  return gtk_clipboard_wait_is_rich_text_available(arg0, arg1);
}

int Pure_gtk_clipboard_wait_is_image_available(GtkClipboard* arg0)
{
  return gtk_clipboard_wait_is_image_available(arg0);
}

int Pure_gtk_clipboard_wait_is_uris_available(GtkClipboard* arg0)
{
  return gtk_clipboard_wait_is_uris_available(arg0);
}

int Pure_gtk_clipboard_wait_is_target_available(GtkClipboard* arg0, struct _GdkAtom* arg1)
{
  return gtk_clipboard_wait_is_target_available(arg0, arg1);
}

void Pure_gtk_clipboard_set_can_store(GtkClipboard* arg0, GtkTargetEntry const* arg1, int arg2)
{
  return gtk_clipboard_set_can_store(arg0, arg1, arg2);
}

void Pure_gtk_clipboard_store(GtkClipboard* arg0)
{
  return gtk_clipboard_store(arg0);
}

unsigned long Pure_gtk_color_button_get_type()
{
  return gtk_color_button_get_type();
}

GtkWidget* Pure_gtk_color_button_new()
{
  return gtk_color_button_new();
}

GtkWidget* Pure_gtk_color_button_new_with_color(GdkColor const* arg0)
{
  return gtk_color_button_new_with_color(arg0);
}

void Pure_gtk_color_button_set_color(GtkColorButton* arg0, GdkColor const* arg1)
{
  return gtk_color_button_set_color(arg0, arg1);
}

void Pure_gtk_color_button_set_alpha(GtkColorButton* arg0, unsigned short arg1)
{
  return gtk_color_button_set_alpha(arg0, arg1);
}

void Pure_gtk_color_button_get_color(GtkColorButton* arg0, GdkColor* arg1)
{
  return gtk_color_button_get_color(arg0, arg1);
}

unsigned short Pure_gtk_color_button_get_alpha(GtkColorButton* arg0)
{
  return gtk_color_button_get_alpha(arg0);
}

void Pure_gtk_color_button_set_use_alpha(GtkColorButton* arg0, int arg1)
{
  return gtk_color_button_set_use_alpha(arg0, arg1);
}

int Pure_gtk_color_button_get_use_alpha(GtkColorButton* arg0)
{
  return gtk_color_button_get_use_alpha(arg0);
}

void Pure_gtk_color_button_set_title(GtkColorButton* arg0, char const* arg1)
{
  return gtk_color_button_set_title(arg0, arg1);
}

char const* Pure_gtk_color_button_get_title(GtkColorButton* arg0)
{
  return gtk_color_button_get_title(arg0);
}

unsigned long Pure_gtk_vbox_get_type()
{
  return gtk_vbox_get_type();
}

GtkWidget* Pure_gtk_vbox_new(int arg0, int arg1)
{
  return gtk_vbox_new(arg0, arg1);
}

unsigned long Pure_gtk_color_selection_get_type()
{
  return gtk_color_selection_get_type();
}

GtkWidget* Pure_gtk_color_selection_new()
{
  return gtk_color_selection_new();
}

int Pure_gtk_color_selection_get_has_opacity_control(GtkColorSelection* arg0)
{
  return gtk_color_selection_get_has_opacity_control(arg0);
}

void Pure_gtk_color_selection_set_has_opacity_control(GtkColorSelection* arg0, int arg1)
{
  return gtk_color_selection_set_has_opacity_control(arg0, arg1);
}

int Pure_gtk_color_selection_get_has_palette(GtkColorSelection* arg0)
{
  return gtk_color_selection_get_has_palette(arg0);
}

void Pure_gtk_color_selection_set_has_palette(GtkColorSelection* arg0, int arg1)
{
  return gtk_color_selection_set_has_palette(arg0, arg1);
}

void Pure_gtk_color_selection_set_current_color(GtkColorSelection* arg0, GdkColor const* arg1)
{
  return gtk_color_selection_set_current_color(arg0, arg1);
}

void Pure_gtk_color_selection_set_current_alpha(GtkColorSelection* arg0, unsigned short arg1)
{
  return gtk_color_selection_set_current_alpha(arg0, arg1);
}

void Pure_gtk_color_selection_get_current_color(GtkColorSelection* arg0, GdkColor* arg1)
{
  return gtk_color_selection_get_current_color(arg0, arg1);
}

unsigned short Pure_gtk_color_selection_get_current_alpha(GtkColorSelection* arg0)
{
  return gtk_color_selection_get_current_alpha(arg0);
}

void Pure_gtk_color_selection_set_previous_color(GtkColorSelection* arg0, GdkColor const* arg1)
{
  return gtk_color_selection_set_previous_color(arg0, arg1);
}

void Pure_gtk_color_selection_set_previous_alpha(GtkColorSelection* arg0, unsigned short arg1)
{
  return gtk_color_selection_set_previous_alpha(arg0, arg1);
}

void Pure_gtk_color_selection_get_previous_color(GtkColorSelection* arg0, GdkColor* arg1)
{
  return gtk_color_selection_get_previous_color(arg0, arg1);
}

unsigned short Pure_gtk_color_selection_get_previous_alpha(GtkColorSelection* arg0)
{
  return gtk_color_selection_get_previous_alpha(arg0);
}

int Pure_gtk_color_selection_is_adjusting(GtkColorSelection* arg0)
{
  return gtk_color_selection_is_adjusting(arg0);
}

int Pure_gtk_color_selection_palette_from_string(char const* arg0, GdkColor** arg1, int* arg2)
{
  return gtk_color_selection_palette_from_string(arg0, arg1, arg2);
}

char* Pure_gtk_color_selection_palette_to_string(GdkColor const* arg0, int arg1)
{
  return gtk_color_selection_palette_to_string(arg0, arg1);
}

void* Pure_gtk_color_selection_set_change_palette_hook(void* arg0)
{
  return gtk_color_selection_set_change_palette_hook(arg0);
}

void* Pure_gtk_color_selection_set_change_palette_with_screen_hook(void* arg0)
{
  return gtk_color_selection_set_change_palette_with_screen_hook(arg0);
}

void Pure_gtk_color_selection_set_color(GtkColorSelection* arg0, double* arg1)
{
  return gtk_color_selection_set_color(arg0, arg1);
}

void Pure_gtk_color_selection_get_color(GtkColorSelection* arg0, double* arg1)
{
  return gtk_color_selection_get_color(arg0, arg1);
}

void Pure_gtk_color_selection_set_update_policy(GtkColorSelection* arg0, unsigned int arg1)
{
  return gtk_color_selection_set_update_policy(arg0, arg1);
}

unsigned long Pure_gtk_color_selection_dialog_get_type()
{
  return gtk_color_selection_dialog_get_type();
}

GtkWidget* Pure_gtk_color_selection_dialog_new(char const* arg0)
{
  return gtk_color_selection_dialog_new(arg0);
}

GtkWidget* Pure_gtk_color_selection_dialog_get_color_selection(GtkColorSelectionDialog* arg0)
{
  return gtk_color_selection_dialog_get_color_selection(arg0);
}

void Pure_gtk_drag_get_data(GtkWidget* arg0, GdkDragContext* arg1, struct _GdkAtom* arg2, unsigned int arg3)
{
  return gtk_drag_get_data(arg0, arg1, arg2, arg3);
}

void Pure_gtk_drag_finish(GdkDragContext* arg0, int arg1, int arg2, unsigned int arg3)
{
  return gtk_drag_finish(arg0, arg1, arg2, arg3);
}

GtkWidget* Pure_gtk_drag_get_source_widget(GdkDragContext* arg0)
{
  return gtk_drag_get_source_widget(arg0);
}

void Pure_gtk_drag_highlight(GtkWidget* arg0)
{
  return gtk_drag_highlight(arg0);
}

void Pure_gtk_drag_unhighlight(GtkWidget* arg0)
{
  return gtk_drag_unhighlight(arg0);
}

void Pure_gtk_drag_dest_set(GtkWidget* arg0, unsigned int arg1, GtkTargetEntry const* arg2, int arg3, unsigned int arg4)
{
  return gtk_drag_dest_set(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_drag_dest_set_proxy(GtkWidget* arg0, GdkWindow* arg1, unsigned int arg2, int arg3)
{
  return gtk_drag_dest_set_proxy(arg0, arg1, arg2, arg3);
}

void Pure_gtk_drag_dest_unset(GtkWidget* arg0)
{
  return gtk_drag_dest_unset(arg0);
}

struct _GdkAtom* Pure_gtk_drag_dest_find_target(GtkWidget* arg0, GdkDragContext* arg1, GtkTargetList* arg2)
{
  return gtk_drag_dest_find_target(arg0, arg1, arg2);
}

GtkTargetList* Pure_gtk_drag_dest_get_target_list(GtkWidget* arg0)
{
  return gtk_drag_dest_get_target_list(arg0);
}

void Pure_gtk_drag_dest_set_target_list(GtkWidget* arg0, GtkTargetList* arg1)
{
  return gtk_drag_dest_set_target_list(arg0, arg1);
}

void Pure_gtk_drag_dest_add_text_targets(GtkWidget* arg0)
{
  return gtk_drag_dest_add_text_targets(arg0);
}

void Pure_gtk_drag_dest_add_image_targets(GtkWidget* arg0)
{
  return gtk_drag_dest_add_image_targets(arg0);
}

void Pure_gtk_drag_dest_add_uri_targets(GtkWidget* arg0)
{
  return gtk_drag_dest_add_uri_targets(arg0);
}

void Pure_gtk_drag_dest_set_track_motion(GtkWidget* arg0, int arg1)
{
  return gtk_drag_dest_set_track_motion(arg0, arg1);
}

int Pure_gtk_drag_dest_get_track_motion(GtkWidget* arg0)
{
  return gtk_drag_dest_get_track_motion(arg0);
}

void Pure_gtk_drag_source_set(GtkWidget* arg0, unsigned int arg1, GtkTargetEntry const* arg2, int arg3, unsigned int arg4)
{
  return gtk_drag_source_set(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_drag_source_unset(GtkWidget* arg0)
{
  return gtk_drag_source_unset(arg0);
}

GtkTargetList* Pure_gtk_drag_source_get_target_list(GtkWidget* arg0)
{
  return gtk_drag_source_get_target_list(arg0);
}

void Pure_gtk_drag_source_set_target_list(GtkWidget* arg0, GtkTargetList* arg1)
{
  return gtk_drag_source_set_target_list(arg0, arg1);
}

void Pure_gtk_drag_source_add_text_targets(GtkWidget* arg0)
{
  return gtk_drag_source_add_text_targets(arg0);
}

void Pure_gtk_drag_source_add_image_targets(GtkWidget* arg0)
{
  return gtk_drag_source_add_image_targets(arg0);
}

void Pure_gtk_drag_source_add_uri_targets(GtkWidget* arg0)
{
  return gtk_drag_source_add_uri_targets(arg0);
}

void Pure_gtk_drag_source_set_icon(GtkWidget* arg0, GdkColormap* arg1, GdkPixmap* arg2, GdkBitmap* arg3)
{
  return gtk_drag_source_set_icon(arg0, arg1, arg2, arg3);
}

void Pure_gtk_drag_source_set_icon_pixbuf(GtkWidget* arg0, GdkPixbuf* arg1)
{
  return gtk_drag_source_set_icon_pixbuf(arg0, arg1);
}

void Pure_gtk_drag_source_set_icon_stock(GtkWidget* arg0, char const* arg1)
{
  return gtk_drag_source_set_icon_stock(arg0, arg1);
}

void Pure_gtk_drag_source_set_icon_name(GtkWidget* arg0, char const* arg1)
{
  return gtk_drag_source_set_icon_name(arg0, arg1);
}

GdkDragContext* Pure_gtk_drag_begin(GtkWidget* arg0, GtkTargetList* arg1, unsigned int arg2, int arg3, GdkEvent* arg4)
{
  return gtk_drag_begin(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_drag_set_icon_widget(GdkDragContext* arg0, GtkWidget* arg1, int arg2, int arg3)
{
  return gtk_drag_set_icon_widget(arg0, arg1, arg2, arg3);
}

void Pure_gtk_drag_set_icon_pixmap(GdkDragContext* arg0, GdkColormap* arg1, GdkPixmap* arg2, GdkBitmap* arg3, int arg4, int arg5)
{
  return gtk_drag_set_icon_pixmap(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_drag_set_icon_pixbuf(GdkDragContext* arg0, GdkPixbuf* arg1, int arg2, int arg3)
{
  return gtk_drag_set_icon_pixbuf(arg0, arg1, arg2, arg3);
}

void Pure_gtk_drag_set_icon_stock(GdkDragContext* arg0, char const* arg1, int arg2, int arg3)
{
  return gtk_drag_set_icon_stock(arg0, arg1, arg2, arg3);
}

void Pure_gtk_drag_set_icon_name(GdkDragContext* arg0, char const* arg1, int arg2, int arg3)
{
  return gtk_drag_set_icon_name(arg0, arg1, arg2, arg3);
}

void Pure_gtk_drag_set_icon_default(GdkDragContext* arg0)
{
  return gtk_drag_set_icon_default(arg0);
}

int Pure_gtk_drag_check_threshold(GtkWidget* arg0, int arg1, int arg2, int arg3, int arg4)
{
  return gtk_drag_check_threshold(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_drag_set_default_icon(GdkColormap* arg0, GdkPixmap* arg1, GdkBitmap* arg2, int arg3, int arg4)
{
  return gtk_drag_set_default_icon(arg0, arg1, arg2, arg3, arg4);
}

unsigned long Pure_gtk_editable_get_type()
{
  return gtk_editable_get_type();
}

void Pure_gtk_editable_select_region(GtkEditable* arg0, int arg1, int arg2)
{
  return gtk_editable_select_region(arg0, arg1, arg2);
}

int Pure_gtk_editable_get_selection_bounds(GtkEditable* arg0, int* arg1, int* arg2)
{
  return gtk_editable_get_selection_bounds(arg0, arg1, arg2);
}

void Pure_gtk_editable_insert_text(GtkEditable* arg0, char const* arg1, int arg2, int* arg3)
{
  return gtk_editable_insert_text(arg0, arg1, arg2, arg3);
}

void Pure_gtk_editable_delete_text(GtkEditable* arg0, int arg1, int arg2)
{
  return gtk_editable_delete_text(arg0, arg1, arg2);
}

char* Pure_gtk_editable_get_chars(GtkEditable* arg0, int arg1, int arg2)
{
  return gtk_editable_get_chars(arg0, arg1, arg2);
}

void Pure_gtk_editable_cut_clipboard(GtkEditable* arg0)
{
  return gtk_editable_cut_clipboard(arg0);
}

void Pure_gtk_editable_copy_clipboard(GtkEditable* arg0)
{
  return gtk_editable_copy_clipboard(arg0);
}

void Pure_gtk_editable_paste_clipboard(GtkEditable* arg0)
{
  return gtk_editable_paste_clipboard(arg0);
}

void Pure_gtk_editable_delete_selection(GtkEditable* arg0)
{
  return gtk_editable_delete_selection(arg0);
}

void Pure_gtk_editable_set_position(GtkEditable* arg0, int arg1)
{
  return gtk_editable_set_position(arg0, arg1);
}

int Pure_gtk_editable_get_position(GtkEditable* arg0)
{
  return gtk_editable_get_position(arg0);
}

void Pure_gtk_editable_set_editable(GtkEditable* arg0, int arg1)
{
  return gtk_editable_set_editable(arg0, arg1);
}

int Pure_gtk_editable_get_editable(GtkEditable* arg0)
{
  return gtk_editable_get_editable(arg0);
}

unsigned long Pure_gtk_im_context_get_type()
{
  return gtk_im_context_get_type();
}

void Pure_gtk_im_context_set_client_window(GtkIMContext* arg0, GdkWindow* arg1)
{
  return gtk_im_context_set_client_window(arg0, arg1);
}

void Pure_gtk_im_context_get_preedit_string(GtkIMContext* arg0, char** arg1, PangoAttrList** arg2, int* arg3)
{
  return gtk_im_context_get_preedit_string(arg0, arg1, arg2, arg3);
}

int Pure_gtk_im_context_filter_keypress(GtkIMContext* arg0, GdkEventKey* arg1)
{
  return gtk_im_context_filter_keypress(arg0, arg1);
}

void Pure_gtk_im_context_focus_in(GtkIMContext* arg0)
{
  return gtk_im_context_focus_in(arg0);
}

void Pure_gtk_im_context_focus_out(GtkIMContext* arg0)
{
  return gtk_im_context_focus_out(arg0);
}

void Pure_gtk_im_context_reset(GtkIMContext* arg0)
{
  return gtk_im_context_reset(arg0);
}

void Pure_gtk_im_context_set_cursor_location(GtkIMContext* arg0, GdkRectangle const* arg1)
{
  return gtk_im_context_set_cursor_location(arg0, arg1);
}

void Pure_gtk_im_context_set_use_preedit(GtkIMContext* arg0, int arg1)
{
  return gtk_im_context_set_use_preedit(arg0, arg1);
}

void Pure_gtk_im_context_set_surrounding(GtkIMContext* arg0, char const* arg1, int arg2, int arg3)
{
  return gtk_im_context_set_surrounding(arg0, arg1, arg2, arg3);
}

int Pure_gtk_im_context_get_surrounding(GtkIMContext* arg0, char** arg1, int* arg2)
{
  return gtk_im_context_get_surrounding(arg0, arg1, arg2);
}

int Pure_gtk_im_context_delete_surrounding(GtkIMContext* arg0, int arg1, int arg2)
{
  return gtk_im_context_delete_surrounding(arg0, arg1, arg2);
}

unsigned long Pure_gtk_list_store_get_type()
{
  return gtk_list_store_get_type();
}

GtkListStore* Pure_gtk_list_store_new(int arg0)
{
  return gtk_list_store_new(arg0);
}

GtkListStore* Pure_gtk_list_store_newv(int arg0, unsigned long* arg1)
{
  return gtk_list_store_newv(arg0, arg1);
}

void Pure_gtk_list_store_set_column_types(GtkListStore* arg0, int arg1, unsigned long* arg2)
{
  return gtk_list_store_set_column_types(arg0, arg1, arg2);
}

void Pure_gtk_list_store_set_value(GtkListStore* arg0, GtkTreeIter* arg1, int arg2, GValue* arg3)
{
  return gtk_list_store_set_value(arg0, arg1, arg2, arg3);
}

void Pure_gtk_list_store_set(GtkListStore* arg0, GtkTreeIter* arg1)
{
  return gtk_list_store_set(arg0, arg1);
}

void Pure_gtk_list_store_set_valuesv(GtkListStore* arg0, GtkTreeIter* arg1, int* arg2, GValue* arg3, int arg4)
{
  return gtk_list_store_set_valuesv(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_list_store_set_valist(GtkListStore* arg0, GtkTreeIter* arg1, void* arg2)
{
  return gtk_list_store_set_valist(arg0, arg1, arg2);
}

int Pure_gtk_list_store_remove(GtkListStore* arg0, GtkTreeIter* arg1)
{
  return gtk_list_store_remove(arg0, arg1);
}

void Pure_gtk_list_store_insert(GtkListStore* arg0, GtkTreeIter* arg1, int arg2)
{
  return gtk_list_store_insert(arg0, arg1, arg2);
}

void Pure_gtk_list_store_insert_before(GtkListStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_list_store_insert_before(arg0, arg1, arg2);
}

void Pure_gtk_list_store_insert_after(GtkListStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_list_store_insert_after(arg0, arg1, arg2);
}

void Pure_gtk_list_store_insert_with_values(GtkListStore* arg0, GtkTreeIter* arg1, int arg2)
{
  return gtk_list_store_insert_with_values(arg0, arg1, arg2);
}

void Pure_gtk_list_store_insert_with_valuesv(GtkListStore* arg0, GtkTreeIter* arg1, int arg2, int* arg3, GValue* arg4, int arg5)
{
  return gtk_list_store_insert_with_valuesv(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_list_store_prepend(GtkListStore* arg0, GtkTreeIter* arg1)
{
  return gtk_list_store_prepend(arg0, arg1);
}

void Pure_gtk_list_store_append(GtkListStore* arg0, GtkTreeIter* arg1)
{
  return gtk_list_store_append(arg0, arg1);
}

void Pure_gtk_list_store_clear(GtkListStore* arg0)
{
  return gtk_list_store_clear(arg0);
}

int Pure_gtk_list_store_iter_is_valid(GtkListStore* arg0, GtkTreeIter* arg1)
{
  return gtk_list_store_iter_is_valid(arg0, arg1);
}

void Pure_gtk_list_store_reorder(GtkListStore* arg0, int* arg1)
{
  return gtk_list_store_reorder(arg0, arg1);
}

void Pure_gtk_list_store_swap(GtkListStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_list_store_swap(arg0, arg1, arg2);
}

void Pure_gtk_list_store_move_after(GtkListStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_list_store_move_after(arg0, arg1, arg2);
}

void Pure_gtk_list_store_move_before(GtkListStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_list_store_move_before(arg0, arg1, arg2);
}

unsigned long Pure_gtk_tree_model_filter_get_type()
{
  return gtk_tree_model_filter_get_type();
}

GtkTreeModel* Pure_gtk_tree_model_filter_new(GtkTreeModel* arg0, GtkTreePath* arg1)
{
  return gtk_tree_model_filter_new(arg0, arg1);
}

void Pure_gtk_tree_model_filter_set_visible_func(GtkTreeModelFilter* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_tree_model_filter_set_visible_func(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_model_filter_set_modify_func(GtkTreeModelFilter* arg0, int arg1, unsigned long* arg2, void* arg3, void* arg4, void* arg5)
{
  return gtk_tree_model_filter_set_modify_func(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_tree_model_filter_set_visible_column(GtkTreeModelFilter* arg0, int arg1)
{
  return gtk_tree_model_filter_set_visible_column(arg0, arg1);
}

GtkTreeModel* Pure_gtk_tree_model_filter_get_model(GtkTreeModelFilter* arg0)
{
  return gtk_tree_model_filter_get_model(arg0);
}

int Pure_gtk_tree_model_filter_convert_child_iter_to_iter(GtkTreeModelFilter* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_model_filter_convert_child_iter_to_iter(arg0, arg1, arg2);
}

void Pure_gtk_tree_model_filter_convert_iter_to_child_iter(GtkTreeModelFilter* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_model_filter_convert_iter_to_child_iter(arg0, arg1, arg2);
}

GtkTreePath* Pure_gtk_tree_model_filter_convert_child_path_to_path(GtkTreeModelFilter* arg0, GtkTreePath* arg1)
{
  return gtk_tree_model_filter_convert_child_path_to_path(arg0, arg1);
}

GtkTreePath* Pure_gtk_tree_model_filter_convert_path_to_child_path(GtkTreeModelFilter* arg0, GtkTreePath* arg1)
{
  return gtk_tree_model_filter_convert_path_to_child_path(arg0, arg1);
}

void Pure_gtk_tree_model_filter_refilter(GtkTreeModelFilter* arg0)
{
  return gtk_tree_model_filter_refilter(arg0);
}

void Pure_gtk_tree_model_filter_clear_cache(GtkTreeModelFilter* arg0)
{
  return gtk_tree_model_filter_clear_cache(arg0);
}

unsigned long Pure_gtk_entry_completion_get_type()
{
  return gtk_entry_completion_get_type();
}

GtkEntryCompletion* Pure_gtk_entry_completion_new()
{
  return gtk_entry_completion_new();
}

GtkWidget* Pure_gtk_entry_completion_get_entry(GtkEntryCompletion* arg0)
{
  return gtk_entry_completion_get_entry(arg0);
}

void Pure_gtk_entry_completion_set_model(GtkEntryCompletion* arg0, GtkTreeModel* arg1)
{
  return gtk_entry_completion_set_model(arg0, arg1);
}

GtkTreeModel* Pure_gtk_entry_completion_get_model(GtkEntryCompletion* arg0)
{
  return gtk_entry_completion_get_model(arg0);
}

void Pure_gtk_entry_completion_set_match_func(GtkEntryCompletion* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_entry_completion_set_match_func(arg0, arg1, arg2, arg3);
}

void Pure_gtk_entry_completion_set_minimum_key_length(GtkEntryCompletion* arg0, int arg1)
{
  return gtk_entry_completion_set_minimum_key_length(arg0, arg1);
}

int Pure_gtk_entry_completion_get_minimum_key_length(GtkEntryCompletion* arg0)
{
  return gtk_entry_completion_get_minimum_key_length(arg0);
}

void Pure_gtk_entry_completion_complete(GtkEntryCompletion* arg0)
{
  return gtk_entry_completion_complete(arg0);
}

void Pure_gtk_entry_completion_insert_prefix(GtkEntryCompletion* arg0)
{
  return gtk_entry_completion_insert_prefix(arg0);
}

void Pure_gtk_entry_completion_insert_action_text(GtkEntryCompletion* arg0, int arg1, char const* arg2)
{
  return gtk_entry_completion_insert_action_text(arg0, arg1, arg2);
}

void Pure_gtk_entry_completion_insert_action_markup(GtkEntryCompletion* arg0, int arg1, char const* arg2)
{
  return gtk_entry_completion_insert_action_markup(arg0, arg1, arg2);
}

void Pure_gtk_entry_completion_delete_action(GtkEntryCompletion* arg0, int arg1)
{
  return gtk_entry_completion_delete_action(arg0, arg1);
}

void Pure_gtk_entry_completion_set_inline_completion(GtkEntryCompletion* arg0, int arg1)
{
  return gtk_entry_completion_set_inline_completion(arg0, arg1);
}

int Pure_gtk_entry_completion_get_inline_completion(GtkEntryCompletion* arg0)
{
  return gtk_entry_completion_get_inline_completion(arg0);
}

void Pure_gtk_entry_completion_set_inline_selection(GtkEntryCompletion* arg0, int arg1)
{
  return gtk_entry_completion_set_inline_selection(arg0, arg1);
}

int Pure_gtk_entry_completion_get_inline_selection(GtkEntryCompletion* arg0)
{
  return gtk_entry_completion_get_inline_selection(arg0);
}

void Pure_gtk_entry_completion_set_popup_completion(GtkEntryCompletion* arg0, int arg1)
{
  return gtk_entry_completion_set_popup_completion(arg0, arg1);
}

int Pure_gtk_entry_completion_get_popup_completion(GtkEntryCompletion* arg0)
{
  return gtk_entry_completion_get_popup_completion(arg0);
}

void Pure_gtk_entry_completion_set_popup_set_width(GtkEntryCompletion* arg0, int arg1)
{
  return gtk_entry_completion_set_popup_set_width(arg0, arg1);
}

int Pure_gtk_entry_completion_get_popup_set_width(GtkEntryCompletion* arg0)
{
  return gtk_entry_completion_get_popup_set_width(arg0);
}

void Pure_gtk_entry_completion_set_popup_single_match(GtkEntryCompletion* arg0, int arg1)
{
  return gtk_entry_completion_set_popup_single_match(arg0, arg1);
}

int Pure_gtk_entry_completion_get_popup_single_match(GtkEntryCompletion* arg0)
{
  return gtk_entry_completion_get_popup_single_match(arg0);
}

char const* Pure_gtk_entry_completion_get_completion_prefix(GtkEntryCompletion* arg0)
{
  return gtk_entry_completion_get_completion_prefix(arg0);
}

void Pure_gtk_entry_completion_set_text_column(GtkEntryCompletion* arg0, int arg1)
{
  return gtk_entry_completion_set_text_column(arg0, arg1);
}

int Pure_gtk_entry_completion_get_text_column(GtkEntryCompletion* arg0)
{
  return gtk_entry_completion_get_text_column(arg0);
}

unsigned long Pure_gtk_entry_get_type()
{
  return gtk_entry_get_type();
}

GtkWidget* Pure_gtk_entry_new()
{
  return gtk_entry_new();
}

void Pure_gtk_entry_set_visibility(GtkEntry* arg0, int arg1)
{
  return gtk_entry_set_visibility(arg0, arg1);
}

int Pure_gtk_entry_get_visibility(GtkEntry* arg0)
{
  return gtk_entry_get_visibility(arg0);
}

void Pure_gtk_entry_set_invisible_char(GtkEntry* arg0, unsigned int arg1)
{
  return gtk_entry_set_invisible_char(arg0, arg1);
}

unsigned int Pure_gtk_entry_get_invisible_char(GtkEntry* arg0)
{
  return gtk_entry_get_invisible_char(arg0);
}

void Pure_gtk_entry_set_has_frame(GtkEntry* arg0, int arg1)
{
  return gtk_entry_set_has_frame(arg0, arg1);
}

int Pure_gtk_entry_get_has_frame(GtkEntry* arg0)
{
  return gtk_entry_get_has_frame(arg0);
}

void Pure_gtk_entry_set_inner_border(GtkEntry* arg0, GtkBorder const* arg1)
{
  return gtk_entry_set_inner_border(arg0, arg1);
}

GtkBorder const* Pure_gtk_entry_get_inner_border(GtkEntry* arg0)
{
  return gtk_entry_get_inner_border(arg0);
}

void Pure_gtk_entry_set_overwrite_mode(GtkEntry* arg0, int arg1)
{
  return gtk_entry_set_overwrite_mode(arg0, arg1);
}

int Pure_gtk_entry_get_overwrite_mode(GtkEntry* arg0)
{
  return gtk_entry_get_overwrite_mode(arg0);
}

void Pure_gtk_entry_set_max_length(GtkEntry* arg0, int arg1)
{
  return gtk_entry_set_max_length(arg0, arg1);
}

int Pure_gtk_entry_get_max_length(GtkEntry* arg0)
{
  return gtk_entry_get_max_length(arg0);
}

unsigned short Pure_gtk_entry_get_text_length(GtkEntry* arg0)
{
  return gtk_entry_get_text_length(arg0);
}

void Pure_gtk_entry_set_activates_default(GtkEntry* arg0, int arg1)
{
  return gtk_entry_set_activates_default(arg0, arg1);
}

int Pure_gtk_entry_get_activates_default(GtkEntry* arg0)
{
  return gtk_entry_get_activates_default(arg0);
}

void Pure_gtk_entry_set_width_chars(GtkEntry* arg0, int arg1)
{
  return gtk_entry_set_width_chars(arg0, arg1);
}

int Pure_gtk_entry_get_width_chars(GtkEntry* arg0)
{
  return gtk_entry_get_width_chars(arg0);
}

void Pure_gtk_entry_set_text(GtkEntry* arg0, char const* arg1)
{
  return gtk_entry_set_text(arg0, arg1);
}

char const* Pure_gtk_entry_get_text(GtkEntry* arg0)
{
  return gtk_entry_get_text(arg0);
}

PangoLayout* Pure_gtk_entry_get_layout(GtkEntry* arg0)
{
  return gtk_entry_get_layout(arg0);
}

void Pure_gtk_entry_get_layout_offsets(GtkEntry* arg0, int* arg1, int* arg2)
{
  return gtk_entry_get_layout_offsets(arg0, arg1, arg2);
}

void Pure_gtk_entry_set_alignment(GtkEntry* arg0, float arg1)
{
  return gtk_entry_set_alignment(arg0, arg1);
}

float Pure_gtk_entry_get_alignment(GtkEntry* arg0)
{
  return gtk_entry_get_alignment(arg0);
}

void Pure_gtk_entry_set_completion(GtkEntry* arg0, GtkEntryCompletion* arg1)
{
  return gtk_entry_set_completion(arg0, arg1);
}

GtkEntryCompletion* Pure_gtk_entry_get_completion(GtkEntry* arg0)
{
  return gtk_entry_get_completion(arg0);
}

int Pure_gtk_entry_layout_index_to_text_index(GtkEntry* arg0, int arg1)
{
  return gtk_entry_layout_index_to_text_index(arg0, arg1);
}

int Pure_gtk_entry_text_index_to_layout_index(GtkEntry* arg0, int arg1)
{
  return gtk_entry_text_index_to_layout_index(arg0, arg1);
}

void Pure_gtk_entry_set_cursor_hadjustment(GtkEntry* arg0, GtkAdjustment* arg1)
{
  return gtk_entry_set_cursor_hadjustment(arg0, arg1);
}

GtkAdjustment* Pure_gtk_entry_get_cursor_hadjustment(GtkEntry* arg0)
{
  return gtk_entry_get_cursor_hadjustment(arg0);
}

GtkWidget* Pure_gtk_entry_new_with_max_length(int arg0)
{
  return gtk_entry_new_with_max_length(arg0);
}

void Pure_gtk_entry_append_text(GtkEntry* arg0, char const* arg1)
{
  return gtk_entry_append_text(arg0, arg1);
}

void Pure_gtk_entry_prepend_text(GtkEntry* arg0, char const* arg1)
{
  return gtk_entry_prepend_text(arg0, arg1);
}

void Pure_gtk_entry_set_position(GtkEntry* arg0, int arg1)
{
  return gtk_entry_set_position(arg0, arg1);
}

void Pure_gtk_entry_select_region(GtkEntry* arg0, int arg1, int arg2)
{
  return gtk_entry_select_region(arg0, arg1, arg2);
}

void Pure_gtk_entry_set_editable(GtkEntry* arg0, int arg1)
{
  return gtk_entry_set_editable(arg0, arg1);
}

unsigned long Pure_gtk_tree_view_get_type()
{
  return gtk_tree_view_get_type();
}

GtkWidget* Pure_gtk_tree_view_new()
{
  return gtk_tree_view_new();
}

GtkWidget* Pure_gtk_tree_view_new_with_model(GtkTreeModel* arg0)
{
  return gtk_tree_view_new_with_model(arg0);
}

GtkTreeModel* Pure_gtk_tree_view_get_model(GtkTreeView* arg0)
{
  return gtk_tree_view_get_model(arg0);
}

void Pure_gtk_tree_view_set_model(GtkTreeView* arg0, GtkTreeModel* arg1)
{
  return gtk_tree_view_set_model(arg0, arg1);
}

GtkTreeSelection* Pure_gtk_tree_view_get_selection(GtkTreeView* arg0)
{
  return gtk_tree_view_get_selection(arg0);
}

GtkAdjustment* Pure_gtk_tree_view_get_hadjustment(GtkTreeView* arg0)
{
  return gtk_tree_view_get_hadjustment(arg0);
}

void Pure_gtk_tree_view_set_hadjustment(GtkTreeView* arg0, GtkAdjustment* arg1)
{
  return gtk_tree_view_set_hadjustment(arg0, arg1);
}

GtkAdjustment* Pure_gtk_tree_view_get_vadjustment(GtkTreeView* arg0)
{
  return gtk_tree_view_get_vadjustment(arg0);
}

void Pure_gtk_tree_view_set_vadjustment(GtkTreeView* arg0, GtkAdjustment* arg1)
{
  return gtk_tree_view_set_vadjustment(arg0, arg1);
}

int Pure_gtk_tree_view_get_headers_visible(GtkTreeView* arg0)
{
  return gtk_tree_view_get_headers_visible(arg0);
}

void Pure_gtk_tree_view_set_headers_visible(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_set_headers_visible(arg0, arg1);
}

void Pure_gtk_tree_view_columns_autosize(GtkTreeView* arg0)
{
  return gtk_tree_view_columns_autosize(arg0);
}

int Pure_gtk_tree_view_get_headers_clickable(GtkTreeView* arg0)
{
  return gtk_tree_view_get_headers_clickable(arg0);
}

void Pure_gtk_tree_view_set_headers_clickable(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_set_headers_clickable(arg0, arg1);
}

void Pure_gtk_tree_view_set_rules_hint(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_set_rules_hint(arg0, arg1);
}

int Pure_gtk_tree_view_get_rules_hint(GtkTreeView* arg0)
{
  return gtk_tree_view_get_rules_hint(arg0);
}

int Pure_gtk_tree_view_append_column(GtkTreeView* arg0, GtkTreeViewColumn* arg1)
{
  return gtk_tree_view_append_column(arg0, arg1);
}

int Pure_gtk_tree_view_remove_column(GtkTreeView* arg0, GtkTreeViewColumn* arg1)
{
  return gtk_tree_view_remove_column(arg0, arg1);
}

int Pure_gtk_tree_view_insert_column(GtkTreeView* arg0, GtkTreeViewColumn* arg1, int arg2)
{
  return gtk_tree_view_insert_column(arg0, arg1, arg2);
}

int Pure_gtk_tree_view_insert_column_with_attributes(GtkTreeView* arg0, int arg1, char const* arg2, GtkCellRenderer* arg3)
{
  return gtk_tree_view_insert_column_with_attributes(arg0, arg1, arg2, arg3);
}

int Pure_gtk_tree_view_insert_column_with_data_func(GtkTreeView* arg0, int arg1, char const* arg2, GtkCellRenderer* arg3, void* arg4, void* arg5, void* arg6)
{
  return gtk_tree_view_insert_column_with_data_func(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GtkTreeViewColumn* Pure_gtk_tree_view_get_column(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_get_column(arg0, arg1);
}

GList* Pure_gtk_tree_view_get_columns(GtkTreeView* arg0)
{
  return gtk_tree_view_get_columns(arg0);
}

void Pure_gtk_tree_view_move_column_after(GtkTreeView* arg0, GtkTreeViewColumn* arg1, GtkTreeViewColumn* arg2)
{
  return gtk_tree_view_move_column_after(arg0, arg1, arg2);
}

void Pure_gtk_tree_view_set_expander_column(GtkTreeView* arg0, GtkTreeViewColumn* arg1)
{
  return gtk_tree_view_set_expander_column(arg0, arg1);
}

GtkTreeViewColumn* Pure_gtk_tree_view_get_expander_column(GtkTreeView* arg0)
{
  return gtk_tree_view_get_expander_column(arg0);
}

void Pure_gtk_tree_view_set_column_drag_function(GtkTreeView* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_tree_view_set_column_drag_function(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_view_scroll_to_point(GtkTreeView* arg0, int arg1, int arg2)
{
  return gtk_tree_view_scroll_to_point(arg0, arg1, arg2);
}

void Pure_gtk_tree_view_scroll_to_cell(GtkTreeView* arg0, GtkTreePath* arg1, GtkTreeViewColumn* arg2, int arg3, float arg4, float arg5)
{
  return gtk_tree_view_scroll_to_cell(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_tree_view_row_activated(GtkTreeView* arg0, GtkTreePath* arg1, GtkTreeViewColumn* arg2)
{
  return gtk_tree_view_row_activated(arg0, arg1, arg2);
}

void Pure_gtk_tree_view_expand_all(GtkTreeView* arg0)
{
  return gtk_tree_view_expand_all(arg0);
}

void Pure_gtk_tree_view_collapse_all(GtkTreeView* arg0)
{
  return gtk_tree_view_collapse_all(arg0);
}

void Pure_gtk_tree_view_expand_to_path(GtkTreeView* arg0, GtkTreePath* arg1)
{
  return gtk_tree_view_expand_to_path(arg0, arg1);
}

int Pure_gtk_tree_view_expand_row(GtkTreeView* arg0, GtkTreePath* arg1, int arg2)
{
  return gtk_tree_view_expand_row(arg0, arg1, arg2);
}

int Pure_gtk_tree_view_collapse_row(GtkTreeView* arg0, GtkTreePath* arg1)
{
  return gtk_tree_view_collapse_row(arg0, arg1);
}

void Pure_gtk_tree_view_map_expanded_rows(GtkTreeView* arg0, void* arg1, void* arg2)
{
  return gtk_tree_view_map_expanded_rows(arg0, arg1, arg2);
}

int Pure_gtk_tree_view_row_expanded(GtkTreeView* arg0, GtkTreePath* arg1)
{
  return gtk_tree_view_row_expanded(arg0, arg1);
}

void Pure_gtk_tree_view_set_reorderable(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_set_reorderable(arg0, arg1);
}

int Pure_gtk_tree_view_get_reorderable(GtkTreeView* arg0)
{
  return gtk_tree_view_get_reorderable(arg0);
}

void Pure_gtk_tree_view_set_cursor(GtkTreeView* arg0, GtkTreePath* arg1, GtkTreeViewColumn* arg2, int arg3)
{
  return gtk_tree_view_set_cursor(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_view_set_cursor_on_cell(GtkTreeView* arg0, GtkTreePath* arg1, GtkTreeViewColumn* arg2, GtkCellRenderer* arg3, int arg4)
{
  return gtk_tree_view_set_cursor_on_cell(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_tree_view_get_cursor(GtkTreeView* arg0, GtkTreePath** arg1, GtkTreeViewColumn** arg2)
{
  return gtk_tree_view_get_cursor(arg0, arg1, arg2);
}

GdkWindow* Pure_gtk_tree_view_get_bin_window(GtkTreeView* arg0)
{
  return gtk_tree_view_get_bin_window(arg0);
}

int Pure_gtk_tree_view_get_path_at_pos(GtkTreeView* arg0, int arg1, int arg2, GtkTreePath** arg3, GtkTreeViewColumn** arg4, int* arg5, int* arg6)
{
  return gtk_tree_view_get_path_at_pos(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_tree_view_get_cell_area(GtkTreeView* arg0, GtkTreePath* arg1, GtkTreeViewColumn* arg2, GdkRectangle* arg3)
{
  return gtk_tree_view_get_cell_area(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_view_get_background_area(GtkTreeView* arg0, GtkTreePath* arg1, GtkTreeViewColumn* arg2, GdkRectangle* arg3)
{
  return gtk_tree_view_get_background_area(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_view_get_visible_rect(GtkTreeView* arg0, GdkRectangle* arg1)
{
  return gtk_tree_view_get_visible_rect(arg0, arg1);
}

void Pure_gtk_tree_view_widget_to_tree_coords(GtkTreeView* arg0, int arg1, int arg2, int* arg3, int* arg4)
{
  return gtk_tree_view_widget_to_tree_coords(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_tree_view_tree_to_widget_coords(GtkTreeView* arg0, int arg1, int arg2, int* arg3, int* arg4)
{
  return gtk_tree_view_tree_to_widget_coords(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gtk_tree_view_get_visible_range(GtkTreeView* arg0, GtkTreePath** arg1, GtkTreePath** arg2)
{
  return gtk_tree_view_get_visible_range(arg0, arg1, arg2);
}

void Pure_gtk_tree_view_enable_model_drag_source(GtkTreeView* arg0, unsigned int arg1, GtkTargetEntry const* arg2, int arg3, unsigned int arg4)
{
  return gtk_tree_view_enable_model_drag_source(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_tree_view_enable_model_drag_dest(GtkTreeView* arg0, GtkTargetEntry const* arg1, int arg2, unsigned int arg3)
{
  return gtk_tree_view_enable_model_drag_dest(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_view_unset_rows_drag_source(GtkTreeView* arg0)
{
  return gtk_tree_view_unset_rows_drag_source(arg0);
}

void Pure_gtk_tree_view_unset_rows_drag_dest(GtkTreeView* arg0)
{
  return gtk_tree_view_unset_rows_drag_dest(arg0);
}

void Pure_gtk_tree_view_set_drag_dest_row(GtkTreeView* arg0, GtkTreePath* arg1, unsigned int arg2)
{
  return gtk_tree_view_set_drag_dest_row(arg0, arg1, arg2);
}

void Pure_gtk_tree_view_get_drag_dest_row(GtkTreeView* arg0, GtkTreePath** arg1, unsigned int* arg2)
{
  return gtk_tree_view_get_drag_dest_row(arg0, arg1, arg2);
}

int Pure_gtk_tree_view_get_dest_row_at_pos(GtkTreeView* arg0, int arg1, int arg2, GtkTreePath** arg3, unsigned int* arg4)
{
  return gtk_tree_view_get_dest_row_at_pos(arg0, arg1, arg2, arg3, arg4);
}

GdkPixmap* Pure_gtk_tree_view_create_row_drag_icon(GtkTreeView* arg0, GtkTreePath* arg1)
{
  return gtk_tree_view_create_row_drag_icon(arg0, arg1);
}

void Pure_gtk_tree_view_set_enable_search(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_set_enable_search(arg0, arg1);
}

int Pure_gtk_tree_view_get_enable_search(GtkTreeView* arg0)
{
  return gtk_tree_view_get_enable_search(arg0);
}

int Pure_gtk_tree_view_get_search_column(GtkTreeView* arg0)
{
  return gtk_tree_view_get_search_column(arg0);
}

void Pure_gtk_tree_view_set_search_column(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_set_search_column(arg0, arg1);
}

void* Pure_gtk_tree_view_get_search_equal_func(GtkTreeView* arg0)
{
  return gtk_tree_view_get_search_equal_func(arg0);
}

void Pure_gtk_tree_view_set_search_equal_func(GtkTreeView* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_tree_view_set_search_equal_func(arg0, arg1, arg2, arg3);
}

GtkEntry* Pure_gtk_tree_view_get_search_entry(GtkTreeView* arg0)
{
  return gtk_tree_view_get_search_entry(arg0);
}

void Pure_gtk_tree_view_set_search_entry(GtkTreeView* arg0, GtkEntry* arg1)
{
  return gtk_tree_view_set_search_entry(arg0, arg1);
}

void* Pure_gtk_tree_view_get_search_position_func(GtkTreeView* arg0)
{
  return gtk_tree_view_get_search_position_func(arg0);
}

void Pure_gtk_tree_view_set_search_position_func(GtkTreeView* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_tree_view_set_search_position_func(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_view_convert_widget_to_tree_coords(GtkTreeView* arg0, int arg1, int arg2, int* arg3, int* arg4)
{
  return gtk_tree_view_convert_widget_to_tree_coords(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_tree_view_convert_tree_to_widget_coords(GtkTreeView* arg0, int arg1, int arg2, int* arg3, int* arg4)
{
  return gtk_tree_view_convert_tree_to_widget_coords(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_tree_view_convert_widget_to_bin_window_coords(GtkTreeView* arg0, int arg1, int arg2, int* arg3, int* arg4)
{
  return gtk_tree_view_convert_widget_to_bin_window_coords(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_tree_view_convert_bin_window_to_widget_coords(GtkTreeView* arg0, int arg1, int arg2, int* arg3, int* arg4)
{
  return gtk_tree_view_convert_bin_window_to_widget_coords(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_tree_view_convert_tree_to_bin_window_coords(GtkTreeView* arg0, int arg1, int arg2, int* arg3, int* arg4)
{
  return gtk_tree_view_convert_tree_to_bin_window_coords(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_tree_view_convert_bin_window_to_tree_coords(GtkTreeView* arg0, int arg1, int arg2, int* arg3, int* arg4)
{
  return gtk_tree_view_convert_bin_window_to_tree_coords(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_tree_view_set_destroy_count_func(GtkTreeView* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_tree_view_set_destroy_count_func(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_view_set_fixed_height_mode(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_set_fixed_height_mode(arg0, arg1);
}

int Pure_gtk_tree_view_get_fixed_height_mode(GtkTreeView* arg0)
{
  return gtk_tree_view_get_fixed_height_mode(arg0);
}

void Pure_gtk_tree_view_set_hover_selection(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_set_hover_selection(arg0, arg1);
}

int Pure_gtk_tree_view_get_hover_selection(GtkTreeView* arg0)
{
  return gtk_tree_view_get_hover_selection(arg0);
}

void Pure_gtk_tree_view_set_hover_expand(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_set_hover_expand(arg0, arg1);
}

int Pure_gtk_tree_view_get_hover_expand(GtkTreeView* arg0)
{
  return gtk_tree_view_get_hover_expand(arg0);
}

void Pure_gtk_tree_view_set_rubber_banding(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_set_rubber_banding(arg0, arg1);
}

int Pure_gtk_tree_view_get_rubber_banding(GtkTreeView* arg0)
{
  return gtk_tree_view_get_rubber_banding(arg0);
}

int Pure_gtk_tree_view_is_rubber_banding_active(GtkTreeView* arg0)
{
  return gtk_tree_view_is_rubber_banding_active(arg0);
}

void* Pure_gtk_tree_view_get_row_separator_func(GtkTreeView* arg0)
{
  return gtk_tree_view_get_row_separator_func(arg0);
}

void Pure_gtk_tree_view_set_row_separator_func(GtkTreeView* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_tree_view_set_row_separator_func(arg0, arg1, arg2, arg3);
}

unsigned int Pure_gtk_tree_view_get_grid_lines(GtkTreeView* arg0)
{
  return gtk_tree_view_get_grid_lines(arg0);
}

void Pure_gtk_tree_view_set_grid_lines(GtkTreeView* arg0, unsigned int arg1)
{
  return gtk_tree_view_set_grid_lines(arg0, arg1);
}

int Pure_gtk_tree_view_get_enable_tree_lines(GtkTreeView* arg0)
{
  return gtk_tree_view_get_enable_tree_lines(arg0);
}

void Pure_gtk_tree_view_set_enable_tree_lines(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_set_enable_tree_lines(arg0, arg1);
}

void Pure_gtk_tree_view_set_show_expanders(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_set_show_expanders(arg0, arg1);
}

int Pure_gtk_tree_view_get_show_expanders(GtkTreeView* arg0)
{
  return gtk_tree_view_get_show_expanders(arg0);
}

void Pure_gtk_tree_view_set_level_indentation(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_set_level_indentation(arg0, arg1);
}

int Pure_gtk_tree_view_get_level_indentation(GtkTreeView* arg0)
{
  return gtk_tree_view_get_level_indentation(arg0);
}

void Pure_gtk_tree_view_set_tooltip_row(GtkTreeView* arg0, GtkTooltip* arg1, GtkTreePath* arg2)
{
  return gtk_tree_view_set_tooltip_row(arg0, arg1, arg2);
}

void Pure_gtk_tree_view_set_tooltip_cell(GtkTreeView* arg0, GtkTooltip* arg1, GtkTreePath* arg2, GtkTreeViewColumn* arg3, GtkCellRenderer* arg4)
{
  return gtk_tree_view_set_tooltip_cell(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gtk_tree_view_get_tooltip_context(GtkTreeView* arg0, int* arg1, int* arg2, int arg3, GtkTreeModel** arg4, GtkTreePath** arg5, GtkTreeIter* arg6)
{
  return gtk_tree_view_get_tooltip_context(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_tree_view_set_tooltip_column(GtkTreeView* arg0, int arg1)
{
  return gtk_tree_view_set_tooltip_column(arg0, arg1);
}

int Pure_gtk_tree_view_get_tooltip_column(GtkTreeView* arg0)
{
  return gtk_tree_view_get_tooltip_column(arg0);
}

unsigned long Pure_gtk_combo_box_get_type()
{
  return gtk_combo_box_get_type();
}

GtkWidget* Pure_gtk_combo_box_new()
{
  return gtk_combo_box_new();
}

GtkWidget* Pure_gtk_combo_box_new_with_model(GtkTreeModel* arg0)
{
  return gtk_combo_box_new_with_model(arg0);
}

int Pure_gtk_combo_box_get_wrap_width(GtkComboBox* arg0)
{
  return gtk_combo_box_get_wrap_width(arg0);
}

void Pure_gtk_combo_box_set_wrap_width(GtkComboBox* arg0, int arg1)
{
  return gtk_combo_box_set_wrap_width(arg0, arg1);
}

int Pure_gtk_combo_box_get_row_span_column(GtkComboBox* arg0)
{
  return gtk_combo_box_get_row_span_column(arg0);
}

void Pure_gtk_combo_box_set_row_span_column(GtkComboBox* arg0, int arg1)
{
  return gtk_combo_box_set_row_span_column(arg0, arg1);
}

int Pure_gtk_combo_box_get_column_span_column(GtkComboBox* arg0)
{
  return gtk_combo_box_get_column_span_column(arg0);
}

void Pure_gtk_combo_box_set_column_span_column(GtkComboBox* arg0, int arg1)
{
  return gtk_combo_box_set_column_span_column(arg0, arg1);
}

int Pure_gtk_combo_box_get_add_tearoffs(GtkComboBox* arg0)
{
  return gtk_combo_box_get_add_tearoffs(arg0);
}

void Pure_gtk_combo_box_set_add_tearoffs(GtkComboBox* arg0, int arg1)
{
  return gtk_combo_box_set_add_tearoffs(arg0, arg1);
}

char const* Pure_gtk_combo_box_get_title(GtkComboBox* arg0)
{
  return gtk_combo_box_get_title(arg0);
}

void Pure_gtk_combo_box_set_title(GtkComboBox* arg0, char const* arg1)
{
  return gtk_combo_box_set_title(arg0, arg1);
}

int Pure_gtk_combo_box_get_focus_on_click(GtkComboBox* arg0)
{
  return gtk_combo_box_get_focus_on_click(arg0);
}

void Pure_gtk_combo_box_set_focus_on_click(GtkComboBox* arg0, int arg1)
{
  return gtk_combo_box_set_focus_on_click(arg0, arg1);
}

int Pure_gtk_combo_box_get_active(GtkComboBox* arg0)
{
  return gtk_combo_box_get_active(arg0);
}

void Pure_gtk_combo_box_set_active(GtkComboBox* arg0, int arg1)
{
  return gtk_combo_box_set_active(arg0, arg1);
}

int Pure_gtk_combo_box_get_active_iter(GtkComboBox* arg0, GtkTreeIter* arg1)
{
  return gtk_combo_box_get_active_iter(arg0, arg1);
}

void Pure_gtk_combo_box_set_active_iter(GtkComboBox* arg0, GtkTreeIter* arg1)
{
  return gtk_combo_box_set_active_iter(arg0, arg1);
}

void Pure_gtk_combo_box_set_model(GtkComboBox* arg0, GtkTreeModel* arg1)
{
  return gtk_combo_box_set_model(arg0, arg1);
}

GtkTreeModel* Pure_gtk_combo_box_get_model(GtkComboBox* arg0)
{
  return gtk_combo_box_get_model(arg0);
}

void* Pure_gtk_combo_box_get_row_separator_func(GtkComboBox* arg0)
{
  return gtk_combo_box_get_row_separator_func(arg0);
}

void Pure_gtk_combo_box_set_row_separator_func(GtkComboBox* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_combo_box_set_row_separator_func(arg0, arg1, arg2, arg3);
}

void Pure_gtk_combo_box_set_button_sensitivity(GtkComboBox* arg0, unsigned int arg1)
{
  return gtk_combo_box_set_button_sensitivity(arg0, arg1);
}

unsigned int Pure_gtk_combo_box_get_button_sensitivity(GtkComboBox* arg0)
{
  return gtk_combo_box_get_button_sensitivity(arg0);
}

GtkWidget* Pure_gtk_combo_box_new_text()
{
  return gtk_combo_box_new_text();
}

void Pure_gtk_combo_box_append_text(GtkComboBox* arg0, char const* arg1)
{
  return gtk_combo_box_append_text(arg0, arg1);
}

void Pure_gtk_combo_box_insert_text(GtkComboBox* arg0, int arg1, char const* arg2)
{
  return gtk_combo_box_insert_text(arg0, arg1, arg2);
}

void Pure_gtk_combo_box_prepend_text(GtkComboBox* arg0, char const* arg1)
{
  return gtk_combo_box_prepend_text(arg0, arg1);
}

void Pure_gtk_combo_box_remove_text(GtkComboBox* arg0, int arg1)
{
  return gtk_combo_box_remove_text(arg0, arg1);
}

char* Pure_gtk_combo_box_get_active_text(GtkComboBox* arg0)
{
  return gtk_combo_box_get_active_text(arg0);
}

void Pure_gtk_combo_box_popup(GtkComboBox* arg0)
{
  return gtk_combo_box_popup(arg0);
}

void Pure_gtk_combo_box_popdown(GtkComboBox* arg0)
{
  return gtk_combo_box_popdown(arg0);
}

AtkObject* Pure_gtk_combo_box_get_popup_accessible(GtkComboBox* arg0)
{
  return gtk_combo_box_get_popup_accessible(arg0);
}

unsigned long Pure_gtk_combo_box_entry_get_type()
{
  return gtk_combo_box_entry_get_type();
}

GtkWidget* Pure_gtk_combo_box_entry_new()
{
  return gtk_combo_box_entry_new();
}

GtkWidget* Pure_gtk_combo_box_entry_new_with_model(GtkTreeModel* arg0, int arg1)
{
  return gtk_combo_box_entry_new_with_model(arg0, arg1);
}

void Pure_gtk_combo_box_entry_set_text_column(GtkComboBoxEntry* arg0, int arg1)
{
  return gtk_combo_box_entry_set_text_column(arg0, arg1);
}

int Pure_gtk_combo_box_entry_get_text_column(GtkComboBoxEntry* arg0)
{
  return gtk_combo_box_entry_get_text_column(arg0);
}

GtkWidget* Pure_gtk_combo_box_entry_new_text()
{
  return gtk_combo_box_entry_new_text();
}

unsigned long Pure_gtk_drawing_area_get_type()
{
  return gtk_drawing_area_get_type();
}

GtkWidget* Pure_gtk_drawing_area_new()
{
  return gtk_drawing_area_new();
}

void Pure_gtk_drawing_area_size(GtkDrawingArea* arg0, int arg1, int arg2)
{
  return gtk_drawing_area_size(arg0, arg1, arg2);
}

unsigned long Pure_gtk_curve_get_type()
{
  return gtk_curve_get_type();
}

GtkWidget* Pure_gtk_curve_new()
{
  return gtk_curve_new();
}

void Pure_gtk_curve_reset(GtkCurve* arg0)
{
  return gtk_curve_reset(arg0);
}

void Pure_gtk_curve_set_gamma(GtkCurve* arg0, float arg1)
{
  return gtk_curve_set_gamma(arg0, arg1);
}

void Pure_gtk_curve_set_range(GtkCurve* arg0, float arg1, float arg2, float arg3, float arg4)
{
  return gtk_curve_set_range(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_curve_get_vector(GtkCurve* arg0, int arg1, float* arg2)
{
  return gtk_curve_get_vector(arg0, arg1, arg2);
}

void Pure_gtk_curve_set_vector(GtkCurve* arg0, int arg1, float* arg2)
{
  return gtk_curve_set_vector(arg0, arg1, arg2);
}

void Pure_gtk_curve_set_curve_type(GtkCurve* arg0, unsigned int arg1)
{
  return gtk_curve_set_curve_type(arg0, arg1);
}

unsigned long Pure_gtk_event_box_get_type()
{
  return gtk_event_box_get_type();
}

GtkWidget* Pure_gtk_event_box_new()
{
  return gtk_event_box_new();
}

int Pure_gtk_event_box_get_visible_window(GtkEventBox* arg0)
{
  return gtk_event_box_get_visible_window(arg0);
}

void Pure_gtk_event_box_set_visible_window(GtkEventBox* arg0, int arg1)
{
  return gtk_event_box_set_visible_window(arg0, arg1);
}

int Pure_gtk_event_box_get_above_child(GtkEventBox* arg0)
{
  return gtk_event_box_get_above_child(arg0);
}

void Pure_gtk_event_box_set_above_child(GtkEventBox* arg0, int arg1)
{
  return gtk_event_box_set_above_child(arg0, arg1);
}

unsigned long Pure_gtk_expander_get_type()
{
  return gtk_expander_get_type();
}

GtkWidget* Pure_gtk_expander_new(char const* arg0)
{
  return gtk_expander_new(arg0);
}

GtkWidget* Pure_gtk_expander_new_with_mnemonic(char const* arg0)
{
  return gtk_expander_new_with_mnemonic(arg0);
}

void Pure_gtk_expander_set_expanded(GtkExpander* arg0, int arg1)
{
  return gtk_expander_set_expanded(arg0, arg1);
}

int Pure_gtk_expander_get_expanded(GtkExpander* arg0)
{
  return gtk_expander_get_expanded(arg0);
}

void Pure_gtk_expander_set_spacing(GtkExpander* arg0, int arg1)
{
  return gtk_expander_set_spacing(arg0, arg1);
}

int Pure_gtk_expander_get_spacing(GtkExpander* arg0)
{
  return gtk_expander_get_spacing(arg0);
}

void Pure_gtk_expander_set_label(GtkExpander* arg0, char const* arg1)
{
  return gtk_expander_set_label(arg0, arg1);
}

char const* Pure_gtk_expander_get_label(GtkExpander* arg0)
{
  return gtk_expander_get_label(arg0);
}

void Pure_gtk_expander_set_use_underline(GtkExpander* arg0, int arg1)
{
  return gtk_expander_set_use_underline(arg0, arg1);
}

int Pure_gtk_expander_get_use_underline(GtkExpander* arg0)
{
  return gtk_expander_get_use_underline(arg0);
}

void Pure_gtk_expander_set_use_markup(GtkExpander* arg0, int arg1)
{
  return gtk_expander_set_use_markup(arg0, arg1);
}

int Pure_gtk_expander_get_use_markup(GtkExpander* arg0)
{
  return gtk_expander_get_use_markup(arg0);
}

void Pure_gtk_expander_set_label_widget(GtkExpander* arg0, GtkWidget* arg1)
{
  return gtk_expander_set_label_widget(arg0, arg1);
}

GtkWidget* Pure_gtk_expander_get_label_widget(GtkExpander* arg0)
{
  return gtk_expander_get_label_widget(arg0);
}

unsigned long Pure_gtk_fixed_get_type()
{
  return gtk_fixed_get_type();
}

GtkWidget* Pure_gtk_fixed_new()
{
  return gtk_fixed_new();
}

void Pure_gtk_fixed_put(GtkFixed* arg0, GtkWidget* arg1, int arg2, int arg3)
{
  return gtk_fixed_put(arg0, arg1, arg2, arg3);
}

void Pure_gtk_fixed_move(GtkFixed* arg0, GtkWidget* arg1, int arg2, int arg3)
{
  return gtk_fixed_move(arg0, arg1, arg2, arg3);
}

void Pure_gtk_fixed_set_has_window(GtkFixed* arg0, int arg1)
{
  return gtk_fixed_set_has_window(arg0, arg1);
}

int Pure_gtk_fixed_get_has_window(GtkFixed* arg0)
{
  return gtk_fixed_get_has_window(arg0);
}

unsigned long Pure_gtk_file_filter_get_type()
{
  return gtk_file_filter_get_type();
}

GtkFileFilter* Pure_gtk_file_filter_new()
{
  return gtk_file_filter_new();
}

void Pure_gtk_file_filter_set_name(GtkFileFilter* arg0, char const* arg1)
{
  return gtk_file_filter_set_name(arg0, arg1);
}

char const* Pure_gtk_file_filter_get_name(GtkFileFilter* arg0)
{
  return gtk_file_filter_get_name(arg0);
}

void Pure_gtk_file_filter_add_mime_type(GtkFileFilter* arg0, char const* arg1)
{
  return gtk_file_filter_add_mime_type(arg0, arg1);
}

void Pure_gtk_file_filter_add_pattern(GtkFileFilter* arg0, char const* arg1)
{
  return gtk_file_filter_add_pattern(arg0, arg1);
}

void Pure_gtk_file_filter_add_pixbuf_formats(GtkFileFilter* arg0)
{
  return gtk_file_filter_add_pixbuf_formats(arg0);
}

void Pure_gtk_file_filter_add_custom(GtkFileFilter* arg0, unsigned int arg1, void* arg2, void* arg3, void* arg4)
{
  return gtk_file_filter_add_custom(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_gtk_file_filter_get_needed(GtkFileFilter* arg0)
{
  return gtk_file_filter_get_needed(arg0);
}

int Pure_gtk_file_filter_filter(GtkFileFilter* arg0, GtkFileFilterInfo const* arg1)
{
  return gtk_file_filter_filter(arg0, arg1);
}

unsigned long Pure_gtk_file_chooser_get_type()
{
  return gtk_file_chooser_get_type();
}

unsigned int Pure_gtk_file_chooser_error_quark()
{
  return gtk_file_chooser_error_quark();
}

void Pure_gtk_file_chooser_set_action(GtkFileChooser* arg0, unsigned int arg1)
{
  return gtk_file_chooser_set_action(arg0, arg1);
}

unsigned int Pure_gtk_file_chooser_get_action(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_action(arg0);
}

void Pure_gtk_file_chooser_set_local_only(GtkFileChooser* arg0, int arg1)
{
  return gtk_file_chooser_set_local_only(arg0, arg1);
}

int Pure_gtk_file_chooser_get_local_only(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_local_only(arg0);
}

void Pure_gtk_file_chooser_set_select_multiple(GtkFileChooser* arg0, int arg1)
{
  return gtk_file_chooser_set_select_multiple(arg0, arg1);
}

int Pure_gtk_file_chooser_get_select_multiple(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_select_multiple(arg0);
}

void Pure_gtk_file_chooser_set_show_hidden(GtkFileChooser* arg0, int arg1)
{
  return gtk_file_chooser_set_show_hidden(arg0, arg1);
}

int Pure_gtk_file_chooser_get_show_hidden(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_show_hidden(arg0);
}

void Pure_gtk_file_chooser_set_do_overwrite_confirmation(GtkFileChooser* arg0, int arg1)
{
  return gtk_file_chooser_set_do_overwrite_confirmation(arg0, arg1);
}

int Pure_gtk_file_chooser_get_do_overwrite_confirmation(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_do_overwrite_confirmation(arg0);
}

void Pure_gtk_file_chooser_set_current_name(GtkFileChooser* arg0, char const* arg1)
{
  return gtk_file_chooser_set_current_name(arg0, arg1);
}

char* Pure_gtk_file_chooser_get_filename(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_filename(arg0);
}

int Pure_gtk_file_chooser_set_filename(GtkFileChooser* arg0, char const* arg1)
{
  return gtk_file_chooser_set_filename(arg0, arg1);
}

int Pure_gtk_file_chooser_select_filename(GtkFileChooser* arg0, char const* arg1)
{
  return gtk_file_chooser_select_filename(arg0, arg1);
}

void Pure_gtk_file_chooser_unselect_filename(GtkFileChooser* arg0, char const* arg1)
{
  return gtk_file_chooser_unselect_filename(arg0, arg1);
}

void Pure_gtk_file_chooser_select_all(GtkFileChooser* arg0)
{
  return gtk_file_chooser_select_all(arg0);
}

void Pure_gtk_file_chooser_unselect_all(GtkFileChooser* arg0)
{
  return gtk_file_chooser_unselect_all(arg0);
}

GSList* Pure_gtk_file_chooser_get_filenames(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_filenames(arg0);
}

int Pure_gtk_file_chooser_set_current_folder(GtkFileChooser* arg0, char const* arg1)
{
  return gtk_file_chooser_set_current_folder(arg0, arg1);
}

char* Pure_gtk_file_chooser_get_current_folder(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_current_folder(arg0);
}

char* Pure_gtk_file_chooser_get_uri(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_uri(arg0);
}

int Pure_gtk_file_chooser_set_uri(GtkFileChooser* arg0, char const* arg1)
{
  return gtk_file_chooser_set_uri(arg0, arg1);
}

int Pure_gtk_file_chooser_select_uri(GtkFileChooser* arg0, char const* arg1)
{
  return gtk_file_chooser_select_uri(arg0, arg1);
}

void Pure_gtk_file_chooser_unselect_uri(GtkFileChooser* arg0, char const* arg1)
{
  return gtk_file_chooser_unselect_uri(arg0, arg1);
}

GSList* Pure_gtk_file_chooser_get_uris(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_uris(arg0);
}

int Pure_gtk_file_chooser_set_current_folder_uri(GtkFileChooser* arg0, char const* arg1)
{
  return gtk_file_chooser_set_current_folder_uri(arg0, arg1);
}

char* Pure_gtk_file_chooser_get_current_folder_uri(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_current_folder_uri(arg0);
}

GFile* Pure_gtk_file_chooser_get_file(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_file(arg0);
}

int Pure_gtk_file_chooser_set_file(GtkFileChooser* arg0, GFile* arg1, GError** arg2)
{
  return gtk_file_chooser_set_file(arg0, arg1, arg2);
}

int Pure_gtk_file_chooser_select_file(GtkFileChooser* arg0, GFile* arg1, GError** arg2)
{
  return gtk_file_chooser_select_file(arg0, arg1, arg2);
}

void Pure_gtk_file_chooser_unselect_file(GtkFileChooser* arg0, GFile* arg1)
{
  return gtk_file_chooser_unselect_file(arg0, arg1);
}

GSList* Pure_gtk_file_chooser_get_files(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_files(arg0);
}

int Pure_gtk_file_chooser_set_current_folder_file(GtkFileChooser* arg0, GFile* arg1, GError** arg2)
{
  return gtk_file_chooser_set_current_folder_file(arg0, arg1, arg2);
}

GFile* Pure_gtk_file_chooser_get_current_folder_file(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_current_folder_file(arg0);
}

void Pure_gtk_file_chooser_set_preview_widget(GtkFileChooser* arg0, GtkWidget* arg1)
{
  return gtk_file_chooser_set_preview_widget(arg0, arg1);
}

GtkWidget* Pure_gtk_file_chooser_get_preview_widget(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_preview_widget(arg0);
}

void Pure_gtk_file_chooser_set_preview_widget_active(GtkFileChooser* arg0, int arg1)
{
  return gtk_file_chooser_set_preview_widget_active(arg0, arg1);
}

int Pure_gtk_file_chooser_get_preview_widget_active(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_preview_widget_active(arg0);
}

void Pure_gtk_file_chooser_set_use_preview_label(GtkFileChooser* arg0, int arg1)
{
  return gtk_file_chooser_set_use_preview_label(arg0, arg1);
}

int Pure_gtk_file_chooser_get_use_preview_label(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_use_preview_label(arg0);
}

char* Pure_gtk_file_chooser_get_preview_filename(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_preview_filename(arg0);
}

char* Pure_gtk_file_chooser_get_preview_uri(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_preview_uri(arg0);
}

GFile* Pure_gtk_file_chooser_get_preview_file(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_preview_file(arg0);
}

void Pure_gtk_file_chooser_set_extra_widget(GtkFileChooser* arg0, GtkWidget* arg1)
{
  return gtk_file_chooser_set_extra_widget(arg0, arg1);
}

GtkWidget* Pure_gtk_file_chooser_get_extra_widget(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_extra_widget(arg0);
}

void Pure_gtk_file_chooser_add_filter(GtkFileChooser* arg0, GtkFileFilter* arg1)
{
  return gtk_file_chooser_add_filter(arg0, arg1);
}

void Pure_gtk_file_chooser_remove_filter(GtkFileChooser* arg0, GtkFileFilter* arg1)
{
  return gtk_file_chooser_remove_filter(arg0, arg1);
}

GSList* Pure_gtk_file_chooser_list_filters(GtkFileChooser* arg0)
{
  return gtk_file_chooser_list_filters(arg0);
}

void Pure_gtk_file_chooser_set_filter(GtkFileChooser* arg0, GtkFileFilter* arg1)
{
  return gtk_file_chooser_set_filter(arg0, arg1);
}

GtkFileFilter* Pure_gtk_file_chooser_get_filter(GtkFileChooser* arg0)
{
  return gtk_file_chooser_get_filter(arg0);
}

int Pure_gtk_file_chooser_add_shortcut_folder(GtkFileChooser* arg0, char const* arg1, GError** arg2)
{
  return gtk_file_chooser_add_shortcut_folder(arg0, arg1, arg2);
}

int Pure_gtk_file_chooser_remove_shortcut_folder(GtkFileChooser* arg0, char const* arg1, GError** arg2)
{
  return gtk_file_chooser_remove_shortcut_folder(arg0, arg1, arg2);
}

GSList* Pure_gtk_file_chooser_list_shortcut_folders(GtkFileChooser* arg0)
{
  return gtk_file_chooser_list_shortcut_folders(arg0);
}

int Pure_gtk_file_chooser_add_shortcut_folder_uri(GtkFileChooser* arg0, char const* arg1, GError** arg2)
{
  return gtk_file_chooser_add_shortcut_folder_uri(arg0, arg1, arg2);
}

int Pure_gtk_file_chooser_remove_shortcut_folder_uri(GtkFileChooser* arg0, char const* arg1, GError** arg2)
{
  return gtk_file_chooser_remove_shortcut_folder_uri(arg0, arg1, arg2);
}

GSList* Pure_gtk_file_chooser_list_shortcut_folder_uris(GtkFileChooser* arg0)
{
  return gtk_file_chooser_list_shortcut_folder_uris(arg0);
}

unsigned long Pure_gtk_hbox_get_type()
{
  return gtk_hbox_get_type();
}

GtkWidget* Pure_gtk_hbox_new(int arg0, int arg1)
{
  return gtk_hbox_new(arg0, arg1);
}

unsigned long Pure_gtk_file_chooser_button_get_type()
{
  return gtk_file_chooser_button_get_type();
}

GtkWidget* Pure_gtk_file_chooser_button_new(char const* arg0, unsigned int arg1)
{
  return gtk_file_chooser_button_new(arg0, arg1);
}

GtkWidget* Pure_gtk_file_chooser_button_new_with_backend(char const* arg0, unsigned int arg1, char const* arg2)
{
  return gtk_file_chooser_button_new_with_backend(arg0, arg1, arg2);
}

GtkWidget* Pure_gtk_file_chooser_button_new_with_dialog(GtkWidget* arg0)
{
  return gtk_file_chooser_button_new_with_dialog(arg0);
}

char const* Pure_gtk_file_chooser_button_get_title(GtkFileChooserButton* arg0)
{
  return gtk_file_chooser_button_get_title(arg0);
}

void Pure_gtk_file_chooser_button_set_title(GtkFileChooserButton* arg0, char const* arg1)
{
  return gtk_file_chooser_button_set_title(arg0, arg1);
}

int Pure_gtk_file_chooser_button_get_width_chars(GtkFileChooserButton* arg0)
{
  return gtk_file_chooser_button_get_width_chars(arg0);
}

void Pure_gtk_file_chooser_button_set_width_chars(GtkFileChooserButton* arg0, int arg1)
{
  return gtk_file_chooser_button_set_width_chars(arg0, arg1);
}

int Pure_gtk_file_chooser_button_get_focus_on_click(GtkFileChooserButton* arg0)
{
  return gtk_file_chooser_button_get_focus_on_click(arg0);
}

void Pure_gtk_file_chooser_button_set_focus_on_click(GtkFileChooserButton* arg0, int arg1)
{
  return gtk_file_chooser_button_set_focus_on_click(arg0, arg1);
}

unsigned long Pure_gtk_file_chooser_dialog_get_type()
{
  return gtk_file_chooser_dialog_get_type();
}

GtkWidget* Pure_gtk_file_chooser_dialog_new(char const* arg0, GtkWindow* arg1, unsigned int arg2, char const* arg3)
{
  return gtk_file_chooser_dialog_new(arg0, arg1, arg2, arg3);
}

GtkWidget* Pure_gtk_file_chooser_dialog_new_with_backend(char const* arg0, GtkWindow* arg1, unsigned int arg2, char const* arg3, char const* arg4)
{
  return gtk_file_chooser_dialog_new_with_backend(arg0, arg1, arg2, arg3, arg4);
}

unsigned long Pure_gtk_file_chooser_widget_get_type()
{
  return gtk_file_chooser_widget_get_type();
}

GtkWidget* Pure_gtk_file_chooser_widget_new(unsigned int arg0)
{
  return gtk_file_chooser_widget_new(arg0);
}

GtkWidget* Pure_gtk_file_chooser_widget_new_with_backend(unsigned int arg0, char const* arg1)
{
  return gtk_file_chooser_widget_new_with_backend(arg0, arg1);
}

unsigned long Pure_gtk_font_button_get_type()
{
  return gtk_font_button_get_type();
}

GtkWidget* Pure_gtk_font_button_new()
{
  return gtk_font_button_new();
}

GtkWidget* Pure_gtk_font_button_new_with_font(char const* arg0)
{
  return gtk_font_button_new_with_font(arg0);
}

char const* Pure_gtk_font_button_get_title(GtkFontButton* arg0)
{
  return gtk_font_button_get_title(arg0);
}

void Pure_gtk_font_button_set_title(GtkFontButton* arg0, char const* arg1)
{
  return gtk_font_button_set_title(arg0, arg1);
}

int Pure_gtk_font_button_get_use_font(GtkFontButton* arg0)
{
  return gtk_font_button_get_use_font(arg0);
}

void Pure_gtk_font_button_set_use_font(GtkFontButton* arg0, int arg1)
{
  return gtk_font_button_set_use_font(arg0, arg1);
}

int Pure_gtk_font_button_get_use_size(GtkFontButton* arg0)
{
  return gtk_font_button_get_use_size(arg0);
}

void Pure_gtk_font_button_set_use_size(GtkFontButton* arg0, int arg1)
{
  return gtk_font_button_set_use_size(arg0, arg1);
}

char const* Pure_gtk_font_button_get_font_name(GtkFontButton* arg0)
{
  return gtk_font_button_get_font_name(arg0);
}

int Pure_gtk_font_button_set_font_name(GtkFontButton* arg0, char const* arg1)
{
  return gtk_font_button_set_font_name(arg0, arg1);
}

int Pure_gtk_font_button_get_show_style(GtkFontButton* arg0)
{
  return gtk_font_button_get_show_style(arg0);
}

void Pure_gtk_font_button_set_show_style(GtkFontButton* arg0, int arg1)
{
  return gtk_font_button_set_show_style(arg0, arg1);
}

int Pure_gtk_font_button_get_show_size(GtkFontButton* arg0)
{
  return gtk_font_button_get_show_size(arg0);
}

void Pure_gtk_font_button_set_show_size(GtkFontButton* arg0, int arg1)
{
  return gtk_font_button_set_show_size(arg0, arg1);
}

unsigned long Pure_gtk_font_selection_get_type()
{
  return gtk_font_selection_get_type();
}

GtkWidget* Pure_gtk_font_selection_new()
{
  return gtk_font_selection_new();
}

GtkWidget* Pure_gtk_font_selection_get_family_list(GtkFontSelection* arg0)
{
  return gtk_font_selection_get_family_list(arg0);
}

GtkWidget* Pure_gtk_font_selection_get_face_list(GtkFontSelection* arg0)
{
  return gtk_font_selection_get_face_list(arg0);
}

GtkWidget* Pure_gtk_font_selection_get_size_entry(GtkFontSelection* arg0)
{
  return gtk_font_selection_get_size_entry(arg0);
}

GtkWidget* Pure_gtk_font_selection_get_size_list(GtkFontSelection* arg0)
{
  return gtk_font_selection_get_size_list(arg0);
}

GtkWidget* Pure_gtk_font_selection_get_preview_entry(GtkFontSelection* arg0)
{
  return gtk_font_selection_get_preview_entry(arg0);
}

PangoFontFamily* Pure_gtk_font_selection_get_family(GtkFontSelection* arg0)
{
  return gtk_font_selection_get_family(arg0);
}

PangoFontFace* Pure_gtk_font_selection_get_face(GtkFontSelection* arg0)
{
  return gtk_font_selection_get_face(arg0);
}

int Pure_gtk_font_selection_get_size(GtkFontSelection* arg0)
{
  return gtk_font_selection_get_size(arg0);
}

char* Pure_gtk_font_selection_get_font_name(GtkFontSelection* arg0)
{
  return gtk_font_selection_get_font_name(arg0);
}

GdkFont* Pure_gtk_font_selection_get_font(GtkFontSelection* arg0)
{
  return gtk_font_selection_get_font(arg0);
}

int Pure_gtk_font_selection_set_font_name(GtkFontSelection* arg0, char const* arg1)
{
  return gtk_font_selection_set_font_name(arg0, arg1);
}

char const* Pure_gtk_font_selection_get_preview_text(GtkFontSelection* arg0)
{
  return gtk_font_selection_get_preview_text(arg0);
}

void Pure_gtk_font_selection_set_preview_text(GtkFontSelection* arg0, char const* arg1)
{
  return gtk_font_selection_set_preview_text(arg0, arg1);
}

unsigned long Pure_gtk_font_selection_dialog_get_type()
{
  return gtk_font_selection_dialog_get_type();
}

GtkWidget* Pure_gtk_font_selection_dialog_new(char const* arg0)
{
  return gtk_font_selection_dialog_new(arg0);
}

GtkWidget* Pure_gtk_font_selection_dialog_get_ok_button(GtkFontSelectionDialog* arg0)
{
  return gtk_font_selection_dialog_get_ok_button(arg0);
}

GtkWidget* Pure_gtk_font_selection_dialog_get_apply_button(GtkFontSelectionDialog* arg0)
{
  return gtk_font_selection_dialog_get_apply_button(arg0);
}

GtkWidget* Pure_gtk_font_selection_dialog_get_cancel_button(GtkFontSelectionDialog* arg0)
{
  return gtk_font_selection_dialog_get_cancel_button(arg0);
}

char* Pure_gtk_font_selection_dialog_get_font_name(GtkFontSelectionDialog* arg0)
{
  return gtk_font_selection_dialog_get_font_name(arg0);
}

GdkFont* Pure_gtk_font_selection_dialog_get_font(GtkFontSelectionDialog* arg0)
{
  return gtk_font_selection_dialog_get_font(arg0);
}

int Pure_gtk_font_selection_dialog_set_font_name(GtkFontSelectionDialog* arg0, char const* arg1)
{
  return gtk_font_selection_dialog_set_font_name(arg0, arg1);
}

char const* Pure_gtk_font_selection_dialog_get_preview_text(GtkFontSelectionDialog* arg0)
{
  return gtk_font_selection_dialog_get_preview_text(arg0);
}

void Pure_gtk_font_selection_dialog_set_preview_text(GtkFontSelectionDialog* arg0, char const* arg1)
{
  return gtk_font_selection_dialog_set_preview_text(arg0, arg1);
}

unsigned long Pure_gtk_gamma_curve_get_type()
{
  return gtk_gamma_curve_get_type();
}

GtkWidget* Pure_gtk_gamma_curve_new()
{
  return gtk_gamma_curve_new();
}

GdkGC* Pure_gtk_gc_get(int arg0, GdkColormap* arg1, GdkGCValues* arg2, unsigned int arg3)
{
  return gtk_gc_get(arg0, arg1, arg2, arg3);
}

void Pure_gtk_gc_release(GdkGC* arg0)
{
  return gtk_gc_release(arg0);
}

unsigned long Pure_gtk_handle_box_get_type()
{
  return gtk_handle_box_get_type();
}

GtkWidget* Pure_gtk_handle_box_new()
{
  return gtk_handle_box_new();
}

void Pure_gtk_handle_box_set_shadow_type(GtkHandleBox* arg0, unsigned int arg1)
{
  return gtk_handle_box_set_shadow_type(arg0, arg1);
}

unsigned int Pure_gtk_handle_box_get_shadow_type(GtkHandleBox* arg0)
{
  return gtk_handle_box_get_shadow_type(arg0);
}

void Pure_gtk_handle_box_set_handle_position(GtkHandleBox* arg0, unsigned int arg1)
{
  return gtk_handle_box_set_handle_position(arg0, arg1);
}

unsigned int Pure_gtk_handle_box_get_handle_position(GtkHandleBox* arg0)
{
  return gtk_handle_box_get_handle_position(arg0);
}

void Pure_gtk_handle_box_set_snap_edge(GtkHandleBox* arg0, unsigned int arg1)
{
  return gtk_handle_box_set_snap_edge(arg0, arg1);
}

unsigned int Pure_gtk_handle_box_get_snap_edge(GtkHandleBox* arg0)
{
  return gtk_handle_box_get_snap_edge(arg0);
}

int Pure_gtk_handle_box_get_child_detached(GtkHandleBox* arg0)
{
  return gtk_handle_box_get_child_detached(arg0);
}

unsigned long Pure_gtk_hbutton_box_get_type()
{
  return gtk_hbutton_box_get_type();
}

GtkWidget* Pure_gtk_hbutton_box_new()
{
  return gtk_hbutton_box_new();
}

int Pure_gtk_hbutton_box_get_spacing_default()
{
  return gtk_hbutton_box_get_spacing_default();
}

unsigned int Pure_gtk_hbutton_box_get_layout_default()
{
  return gtk_hbutton_box_get_layout_default();
}

void Pure_gtk_hbutton_box_set_spacing_default(int arg0)
{
  return gtk_hbutton_box_set_spacing_default(arg0);
}

void Pure_gtk_hbutton_box_set_layout_default(unsigned int arg0)
{
  return gtk_hbutton_box_set_layout_default(arg0);
}

unsigned long Pure_gtk_paned_get_type()
{
  return gtk_paned_get_type();
}

void Pure_gtk_paned_add1(GtkPaned* arg0, GtkWidget* arg1)
{
  return gtk_paned_add1(arg0, arg1);
}

void Pure_gtk_paned_add2(GtkPaned* arg0, GtkWidget* arg1)
{
  return gtk_paned_add2(arg0, arg1);
}

void Pure_gtk_paned_pack1(GtkPaned* arg0, GtkWidget* arg1, int arg2, int arg3)
{
  return gtk_paned_pack1(arg0, arg1, arg2, arg3);
}

void Pure_gtk_paned_pack2(GtkPaned* arg0, GtkWidget* arg1, int arg2, int arg3)
{
  return gtk_paned_pack2(arg0, arg1, arg2, arg3);
}

int Pure_gtk_paned_get_position(GtkPaned* arg0)
{
  return gtk_paned_get_position(arg0);
}

void Pure_gtk_paned_set_position(GtkPaned* arg0, int arg1)
{
  return gtk_paned_set_position(arg0, arg1);
}

GtkWidget* Pure_gtk_paned_get_child1(GtkPaned* arg0)
{
  return gtk_paned_get_child1(arg0);
}

GtkWidget* Pure_gtk_paned_get_child2(GtkPaned* arg0)
{
  return gtk_paned_get_child2(arg0);
}

void Pure_gtk_paned_compute_position(GtkPaned* arg0, int arg1, int arg2, int arg3)
{
  return gtk_paned_compute_position(arg0, arg1, arg2, arg3);
}

unsigned long Pure_gtk_hpaned_get_type()
{
  return gtk_hpaned_get_type();
}

GtkWidget* Pure_gtk_hpaned_new()
{
  return gtk_hpaned_new();
}

unsigned long Pure_gtk_ruler_get_type()
{
  return gtk_ruler_get_type();
}

void Pure_gtk_ruler_set_metric(GtkRuler* arg0, unsigned int arg1)
{
  return gtk_ruler_set_metric(arg0, arg1);
}

void Pure_gtk_ruler_set_range(GtkRuler* arg0, double arg1, double arg2, double arg3, double arg4)
{
  return gtk_ruler_set_range(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_ruler_draw_ticks(GtkRuler* arg0)
{
  return gtk_ruler_draw_ticks(arg0);
}

void Pure_gtk_ruler_draw_pos(GtkRuler* arg0)
{
  return gtk_ruler_draw_pos(arg0);
}

unsigned int Pure_gtk_ruler_get_metric(GtkRuler* arg0)
{
  return gtk_ruler_get_metric(arg0);
}

void Pure_gtk_ruler_get_range(GtkRuler* arg0, double* arg1, double* arg2, double* arg3, double* arg4)
{
  return gtk_ruler_get_range(arg0, arg1, arg2, arg3, arg4);
}

unsigned long Pure_gtk_hruler_get_type()
{
  return gtk_hruler_get_type();
}

GtkWidget* Pure_gtk_hruler_new()
{
  return gtk_hruler_new();
}

unsigned long Pure_gtk_range_get_type()
{
  return gtk_range_get_type();
}

void Pure_gtk_range_set_update_policy(GtkRange* arg0, unsigned int arg1)
{
  return gtk_range_set_update_policy(arg0, arg1);
}

unsigned int Pure_gtk_range_get_update_policy(GtkRange* arg0)
{
  return gtk_range_get_update_policy(arg0);
}

void Pure_gtk_range_set_adjustment(GtkRange* arg0, GtkAdjustment* arg1)
{
  return gtk_range_set_adjustment(arg0, arg1);
}

GtkAdjustment* Pure_gtk_range_get_adjustment(GtkRange* arg0)
{
  return gtk_range_get_adjustment(arg0);
}

void Pure_gtk_range_set_inverted(GtkRange* arg0, int arg1)
{
  return gtk_range_set_inverted(arg0, arg1);
}

int Pure_gtk_range_get_inverted(GtkRange* arg0)
{
  return gtk_range_get_inverted(arg0);
}

void Pure_gtk_range_set_lower_stepper_sensitivity(GtkRange* arg0, unsigned int arg1)
{
  return gtk_range_set_lower_stepper_sensitivity(arg0, arg1);
}

unsigned int Pure_gtk_range_get_lower_stepper_sensitivity(GtkRange* arg0)
{
  return gtk_range_get_lower_stepper_sensitivity(arg0);
}

void Pure_gtk_range_set_upper_stepper_sensitivity(GtkRange* arg0, unsigned int arg1)
{
  return gtk_range_set_upper_stepper_sensitivity(arg0, arg1);
}

unsigned int Pure_gtk_range_get_upper_stepper_sensitivity(GtkRange* arg0)
{
  return gtk_range_get_upper_stepper_sensitivity(arg0);
}

void Pure_gtk_range_set_increments(GtkRange* arg0, double arg1, double arg2)
{
  return gtk_range_set_increments(arg0, arg1, arg2);
}

void Pure_gtk_range_set_range(GtkRange* arg0, double arg1, double arg2)
{
  return gtk_range_set_range(arg0, arg1, arg2);
}

void Pure_gtk_range_set_value(GtkRange* arg0, double arg1)
{
  return gtk_range_set_value(arg0, arg1);
}

double Pure_gtk_range_get_value(GtkRange* arg0)
{
  return gtk_range_get_value(arg0);
}

void Pure_gtk_range_set_show_fill_level(GtkRange* arg0, int arg1)
{
  return gtk_range_set_show_fill_level(arg0, arg1);
}

int Pure_gtk_range_get_show_fill_level(GtkRange* arg0)
{
  return gtk_range_get_show_fill_level(arg0);
}

void Pure_gtk_range_set_restrict_to_fill_level(GtkRange* arg0, int arg1)
{
  return gtk_range_set_restrict_to_fill_level(arg0, arg1);
}

int Pure_gtk_range_get_restrict_to_fill_level(GtkRange* arg0)
{
  return gtk_range_get_restrict_to_fill_level(arg0);
}

void Pure_gtk_range_set_fill_level(GtkRange* arg0, double arg1)
{
  return gtk_range_set_fill_level(arg0, arg1);
}

double Pure_gtk_range_get_fill_level(GtkRange* arg0)
{
  return gtk_range_get_fill_level(arg0);
}

unsigned long Pure_gtk_scale_get_type()
{
  return gtk_scale_get_type();
}

void Pure_gtk_scale_set_digits(GtkScale* arg0, int arg1)
{
  return gtk_scale_set_digits(arg0, arg1);
}

int Pure_gtk_scale_get_digits(GtkScale* arg0)
{
  return gtk_scale_get_digits(arg0);
}

void Pure_gtk_scale_set_draw_value(GtkScale* arg0, int arg1)
{
  return gtk_scale_set_draw_value(arg0, arg1);
}

int Pure_gtk_scale_get_draw_value(GtkScale* arg0)
{
  return gtk_scale_get_draw_value(arg0);
}

void Pure_gtk_scale_set_value_pos(GtkScale* arg0, unsigned int arg1)
{
  return gtk_scale_set_value_pos(arg0, arg1);
}

unsigned int Pure_gtk_scale_get_value_pos(GtkScale* arg0)
{
  return gtk_scale_get_value_pos(arg0);
}

PangoLayout* Pure_gtk_scale_get_layout(GtkScale* arg0)
{
  return gtk_scale_get_layout(arg0);
}

void Pure_gtk_scale_get_layout_offsets(GtkScale* arg0, int* arg1, int* arg2)
{
  return gtk_scale_get_layout_offsets(arg0, arg1, arg2);
}

unsigned long Pure_gtk_hscale_get_type()
{
  return gtk_hscale_get_type();
}

GtkWidget* Pure_gtk_hscale_new(GtkAdjustment* arg0)
{
  return gtk_hscale_new(arg0);
}

GtkWidget* Pure_gtk_hscale_new_with_range(double arg0, double arg1, double arg2)
{
  return gtk_hscale_new_with_range(arg0, arg1, arg2);
}

unsigned long Pure_gtk_scrollbar_get_type()
{
  return gtk_scrollbar_get_type();
}

unsigned long Pure_gtk_hscrollbar_get_type()
{
  return gtk_hscrollbar_get_type();
}

GtkWidget* Pure_gtk_hscrollbar_new(GtkAdjustment* arg0)
{
  return gtk_hscrollbar_new(arg0);
}

unsigned long Pure_gtk_separator_get_type()
{
  return gtk_separator_get_type();
}

unsigned long Pure_gtk_hseparator_get_type()
{
  return gtk_hseparator_get_type();
}

GtkWidget* Pure_gtk_hseparator_new()
{
  return gtk_hseparator_new();
}

unsigned long Pure_gtk_hsv_get_type()
{
  return gtk_hsv_get_type();
}

GtkWidget* Pure_gtk_hsv_new()
{
  return gtk_hsv_new();
}

void Pure_gtk_hsv_set_color(GtkHSV* arg0, double arg1, double arg2, double arg3)
{
  return gtk_hsv_set_color(arg0, arg1, arg2, arg3);
}

void Pure_gtk_hsv_get_color(GtkHSV* arg0, double* arg1, double* arg2, double* arg3)
{
  return gtk_hsv_get_color(arg0, arg1, arg2, arg3);
}

void Pure_gtk_hsv_set_metrics(GtkHSV* arg0, int arg1, int arg2)
{
  return gtk_hsv_set_metrics(arg0, arg1, arg2);
}

void Pure_gtk_hsv_get_metrics(GtkHSV* arg0, int* arg1, int* arg2)
{
  return gtk_hsv_get_metrics(arg0, arg1, arg2);
}

int Pure_gtk_hsv_is_adjusting(GtkHSV* arg0)
{
  return gtk_hsv_is_adjusting(arg0);
}

void Pure_gtk_hsv_to_rgb(double arg0, double arg1, double arg2, double* arg3, double* arg4, double* arg5)
{
  return gtk_hsv_to_rgb(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_rgb_to_hsv(double arg0, double arg1, double arg2, double* arg3, double* arg4, double* arg5)
{
  return gtk_rgb_to_hsv(arg0, arg1, arg2, arg3, arg4, arg5);
}

unsigned long Pure_gtk_icon_factory_get_type()
{
  return gtk_icon_factory_get_type();
}

GtkIconFactory* Pure_gtk_icon_factory_new()
{
  return gtk_icon_factory_new();
}

void Pure_gtk_icon_factory_add(GtkIconFactory* arg0, char const* arg1, GtkIconSet* arg2)
{
  return gtk_icon_factory_add(arg0, arg1, arg2);
}

GtkIconSet* Pure_gtk_icon_factory_lookup(GtkIconFactory* arg0, char const* arg1)
{
  return gtk_icon_factory_lookup(arg0, arg1);
}

void Pure_gtk_icon_factory_add_default(GtkIconFactory* arg0)
{
  return gtk_icon_factory_add_default(arg0);
}

void Pure_gtk_icon_factory_remove_default(GtkIconFactory* arg0)
{
  return gtk_icon_factory_remove_default(arg0);
}

GtkIconSet* Pure_gtk_icon_factory_lookup_default(char const* arg0)
{
  return gtk_icon_factory_lookup_default(arg0);
}

int Pure_gtk_icon_size_lookup(unsigned int arg0, int* arg1, int* arg2)
{
  return gtk_icon_size_lookup(arg0, arg1, arg2);
}

int Pure_gtk_icon_size_lookup_for_settings(GtkSettings* arg0, unsigned int arg1, int* arg2, int* arg3)
{
  return gtk_icon_size_lookup_for_settings(arg0, arg1, arg2, arg3);
}

unsigned int Pure_gtk_icon_size_register(char const* arg0, int arg1, int arg2)
{
  return gtk_icon_size_register(arg0, arg1, arg2);
}

void Pure_gtk_icon_size_register_alias(char const* arg0, unsigned int arg1)
{
  return gtk_icon_size_register_alias(arg0, arg1);
}

unsigned int Pure_gtk_icon_size_from_name(char const* arg0)
{
  return gtk_icon_size_from_name(arg0);
}

char const* Pure_gtk_icon_size_get_name(unsigned int arg0)
{
  return gtk_icon_size_get_name(arg0);
}

unsigned long Pure_gtk_icon_set_get_type()
{
  return gtk_icon_set_get_type();
}

GtkIconSet* Pure_gtk_icon_set_new()
{
  return gtk_icon_set_new();
}

GtkIconSet* Pure_gtk_icon_set_new_from_pixbuf(GdkPixbuf* arg0)
{
  return gtk_icon_set_new_from_pixbuf(arg0);
}

GtkIconSet* Pure_gtk_icon_set_ref(GtkIconSet* arg0)
{
  return gtk_icon_set_ref(arg0);
}

void Pure_gtk_icon_set_unref(GtkIconSet* arg0)
{
  return gtk_icon_set_unref(arg0);
}

GtkIconSet* Pure_gtk_icon_set_copy(GtkIconSet* arg0)
{
  return gtk_icon_set_copy(arg0);
}

GdkPixbuf* Pure_gtk_icon_set_render_icon(GtkIconSet* arg0, GtkStyle* arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, GtkWidget* arg5, char const* arg6)
{
  return gtk_icon_set_render_icon(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_icon_set_add_source(GtkIconSet* arg0, GtkIconSource const* arg1)
{
  return gtk_icon_set_add_source(arg0, arg1);
}

void Pure_gtk_icon_set_get_sizes(GtkIconSet* arg0, unsigned int** arg1, int* arg2)
{
  return gtk_icon_set_get_sizes(arg0, arg1, arg2);
}

unsigned long Pure_gtk_icon_source_get_type()
{
  return gtk_icon_source_get_type();
}

GtkIconSource* Pure_gtk_icon_source_new()
{
  return gtk_icon_source_new();
}

GtkIconSource* Pure_gtk_icon_source_copy(GtkIconSource const* arg0)
{
  return gtk_icon_source_copy(arg0);
}

void Pure_gtk_icon_source_free(GtkIconSource* arg0)
{
  return gtk_icon_source_free(arg0);
}

void Pure_gtk_icon_source_set_filename(GtkIconSource* arg0, char const* arg1)
{
  return gtk_icon_source_set_filename(arg0, arg1);
}

void Pure_gtk_icon_source_set_icon_name(GtkIconSource* arg0, char const* arg1)
{
  return gtk_icon_source_set_icon_name(arg0, arg1);
}

void Pure_gtk_icon_source_set_pixbuf(GtkIconSource* arg0, GdkPixbuf* arg1)
{
  return gtk_icon_source_set_pixbuf(arg0, arg1);
}

char const* Pure_gtk_icon_source_get_filename(GtkIconSource const* arg0)
{
  return gtk_icon_source_get_filename(arg0);
}

char const* Pure_gtk_icon_source_get_icon_name(GtkIconSource const* arg0)
{
  return gtk_icon_source_get_icon_name(arg0);
}

GdkPixbuf* Pure_gtk_icon_source_get_pixbuf(GtkIconSource const* arg0)
{
  return gtk_icon_source_get_pixbuf(arg0);
}

void Pure_gtk_icon_source_set_direction_wildcarded(GtkIconSource* arg0, int arg1)
{
  return gtk_icon_source_set_direction_wildcarded(arg0, arg1);
}

void Pure_gtk_icon_source_set_state_wildcarded(GtkIconSource* arg0, int arg1)
{
  return gtk_icon_source_set_state_wildcarded(arg0, arg1);
}

void Pure_gtk_icon_source_set_size_wildcarded(GtkIconSource* arg0, int arg1)
{
  return gtk_icon_source_set_size_wildcarded(arg0, arg1);
}

int Pure_gtk_icon_source_get_size_wildcarded(GtkIconSource const* arg0)
{
  return gtk_icon_source_get_size_wildcarded(arg0);
}

int Pure_gtk_icon_source_get_state_wildcarded(GtkIconSource const* arg0)
{
  return gtk_icon_source_get_state_wildcarded(arg0);
}

int Pure_gtk_icon_source_get_direction_wildcarded(GtkIconSource const* arg0)
{
  return gtk_icon_source_get_direction_wildcarded(arg0);
}

void Pure_gtk_icon_source_set_direction(GtkIconSource* arg0, unsigned int arg1)
{
  return gtk_icon_source_set_direction(arg0, arg1);
}

void Pure_gtk_icon_source_set_state(GtkIconSource* arg0, unsigned int arg1)
{
  return gtk_icon_source_set_state(arg0, arg1);
}

void Pure_gtk_icon_source_set_size(GtkIconSource* arg0, unsigned int arg1)
{
  return gtk_icon_source_set_size(arg0, arg1);
}

unsigned int Pure_gtk_icon_source_get_direction(GtkIconSource const* arg0)
{
  return gtk_icon_source_get_direction(arg0);
}

unsigned int Pure_gtk_icon_source_get_state(GtkIconSource const* arg0)
{
  return gtk_icon_source_get_state(arg0);
}

unsigned int Pure_gtk_icon_source_get_size(GtkIconSource const* arg0)
{
  return gtk_icon_source_get_size(arg0);
}

unsigned int Pure_gtk_icon_theme_error_quark()
{
  return gtk_icon_theme_error_quark();
}

unsigned long Pure_gtk_icon_theme_get_type()
{
  return gtk_icon_theme_get_type();
}

GtkIconTheme* Pure_gtk_icon_theme_new()
{
  return gtk_icon_theme_new();
}

GtkIconTheme* Pure_gtk_icon_theme_get_default()
{
  return gtk_icon_theme_get_default();
}

GtkIconTheme* Pure_gtk_icon_theme_get_for_screen(GdkScreen* arg0)
{
  return gtk_icon_theme_get_for_screen(arg0);
}

void Pure_gtk_icon_theme_set_screen(GtkIconTheme* arg0, GdkScreen* arg1)
{
  return gtk_icon_theme_set_screen(arg0, arg1);
}

void Pure_gtk_icon_theme_set_search_path(GtkIconTheme* arg0, char const** arg1, int arg2)
{
  return gtk_icon_theme_set_search_path(arg0, arg1, arg2);
}

void Pure_gtk_icon_theme_get_search_path(GtkIconTheme* arg0, char*** arg1, int* arg2)
{
  return gtk_icon_theme_get_search_path(arg0, arg1, arg2);
}

void Pure_gtk_icon_theme_append_search_path(GtkIconTheme* arg0, char const* arg1)
{
  return gtk_icon_theme_append_search_path(arg0, arg1);
}

void Pure_gtk_icon_theme_prepend_search_path(GtkIconTheme* arg0, char const* arg1)
{
  return gtk_icon_theme_prepend_search_path(arg0, arg1);
}

void Pure_gtk_icon_theme_set_custom_theme(GtkIconTheme* arg0, char const* arg1)
{
  return gtk_icon_theme_set_custom_theme(arg0, arg1);
}

int Pure_gtk_icon_theme_has_icon(GtkIconTheme* arg0, char const* arg1)
{
  return gtk_icon_theme_has_icon(arg0, arg1);
}

int* Pure_gtk_icon_theme_get_icon_sizes(GtkIconTheme* arg0, char const* arg1)
{
  return gtk_icon_theme_get_icon_sizes(arg0, arg1);
}

GtkIconInfo* Pure_gtk_icon_theme_lookup_icon(GtkIconTheme* arg0, char const* arg1, int arg2, unsigned int arg3)
{
  return gtk_icon_theme_lookup_icon(arg0, arg1, arg2, arg3);
}

GtkIconInfo* Pure_gtk_icon_theme_choose_icon(GtkIconTheme* arg0, char const** arg1, int arg2, unsigned int arg3)
{
  return gtk_icon_theme_choose_icon(arg0, arg1, arg2, arg3);
}

GdkPixbuf* Pure_gtk_icon_theme_load_icon(GtkIconTheme* arg0, char const* arg1, int arg2, unsigned int arg3, GError** arg4)
{
  return gtk_icon_theme_load_icon(arg0, arg1, arg2, arg3, arg4);
}

GtkIconInfo* Pure_gtk_icon_theme_lookup_by_gicon(GtkIconTheme* arg0, GIcon* arg1, int arg2, unsigned int arg3)
{
  return gtk_icon_theme_lookup_by_gicon(arg0, arg1, arg2, arg3);
}

GList* Pure_gtk_icon_theme_list_icons(GtkIconTheme* arg0, char const* arg1)
{
  return gtk_icon_theme_list_icons(arg0, arg1);
}

GList* Pure_gtk_icon_theme_list_contexts(GtkIconTheme* arg0)
{
  return gtk_icon_theme_list_contexts(arg0);
}

char* Pure_gtk_icon_theme_get_example_icon_name(GtkIconTheme* arg0)
{
  return gtk_icon_theme_get_example_icon_name(arg0);
}

int Pure_gtk_icon_theme_rescan_if_needed(GtkIconTheme* arg0)
{
  return gtk_icon_theme_rescan_if_needed(arg0);
}

void Pure_gtk_icon_theme_add_builtin_icon(char const* arg0, int arg1, GdkPixbuf* arg2)
{
  return gtk_icon_theme_add_builtin_icon(arg0, arg1, arg2);
}

unsigned long Pure_gtk_icon_info_get_type()
{
  return gtk_icon_info_get_type();
}

GtkIconInfo* Pure_gtk_icon_info_copy(GtkIconInfo* arg0)
{
  return gtk_icon_info_copy(arg0);
}

void Pure_gtk_icon_info_free(GtkIconInfo* arg0)
{
  return gtk_icon_info_free(arg0);
}

GtkIconInfo* Pure_gtk_icon_info_new_for_pixbuf(GtkIconTheme* arg0, GdkPixbuf* arg1)
{
  return gtk_icon_info_new_for_pixbuf(arg0, arg1);
}

int Pure_gtk_icon_info_get_base_size(GtkIconInfo* arg0)
{
  return gtk_icon_info_get_base_size(arg0);
}

char const* Pure_gtk_icon_info_get_filename(GtkIconInfo* arg0)
{
  return gtk_icon_info_get_filename(arg0);
}

GdkPixbuf* Pure_gtk_icon_info_get_builtin_pixbuf(GtkIconInfo* arg0)
{
  return gtk_icon_info_get_builtin_pixbuf(arg0);
}

GdkPixbuf* Pure_gtk_icon_info_load_icon(GtkIconInfo* arg0, GError** arg1)
{
  return gtk_icon_info_load_icon(arg0, arg1);
}

void Pure_gtk_icon_info_set_raw_coordinates(GtkIconInfo* arg0, int arg1)
{
  return gtk_icon_info_set_raw_coordinates(arg0, arg1);
}

int Pure_gtk_icon_info_get_embedded_rect(GtkIconInfo* arg0, GdkRectangle* arg1)
{
  return gtk_icon_info_get_embedded_rect(arg0, arg1);
}

int Pure_gtk_icon_info_get_attach_points(GtkIconInfo* arg0, GdkPoint** arg1, int* arg2)
{
  return gtk_icon_info_get_attach_points(arg0, arg1, arg2);
}

char const* Pure_gtk_icon_info_get_display_name(GtkIconInfo* arg0)
{
  return gtk_icon_info_get_display_name(arg0);
}

unsigned long Pure_gtk_tooltip_get_type()
{
  return gtk_tooltip_get_type();
}

void Pure_gtk_tooltip_set_markup(GtkTooltip* arg0, char const* arg1)
{
  return gtk_tooltip_set_markup(arg0, arg1);
}

void Pure_gtk_tooltip_set_text(GtkTooltip* arg0, char const* arg1)
{
  return gtk_tooltip_set_text(arg0, arg1);
}

void Pure_gtk_tooltip_set_icon(GtkTooltip* arg0, GdkPixbuf* arg1)
{
  return gtk_tooltip_set_icon(arg0, arg1);
}

void Pure_gtk_tooltip_set_icon_from_stock(GtkTooltip* arg0, char const* arg1, unsigned int arg2)
{
  return gtk_tooltip_set_icon_from_stock(arg0, arg1, arg2);
}

void Pure_gtk_tooltip_set_icon_from_icon_name(GtkTooltip* arg0, char const* arg1, unsigned int arg2)
{
  return gtk_tooltip_set_icon_from_icon_name(arg0, arg1, arg2);
}

void Pure_gtk_tooltip_set_custom(GtkTooltip* arg0, GtkWidget* arg1)
{
  return gtk_tooltip_set_custom(arg0, arg1);
}

void Pure_gtk_tooltip_set_tip_area(GtkTooltip* arg0, GdkRectangle const* arg1)
{
  return gtk_tooltip_set_tip_area(arg0, arg1);
}

void Pure_gtk_tooltip_trigger_tooltip_query(GdkDisplay* arg0)
{
  return gtk_tooltip_trigger_tooltip_query(arg0);
}

unsigned long Pure_gtk_icon_view_get_type()
{
  return gtk_icon_view_get_type();
}

GtkWidget* Pure_gtk_icon_view_new()
{
  return gtk_icon_view_new();
}

GtkWidget* Pure_gtk_icon_view_new_with_model(GtkTreeModel* arg0)
{
  return gtk_icon_view_new_with_model(arg0);
}

void Pure_gtk_icon_view_set_model(GtkIconView* arg0, GtkTreeModel* arg1)
{
  return gtk_icon_view_set_model(arg0, arg1);
}

GtkTreeModel* Pure_gtk_icon_view_get_model(GtkIconView* arg0)
{
  return gtk_icon_view_get_model(arg0);
}

void Pure_gtk_icon_view_set_text_column(GtkIconView* arg0, int arg1)
{
  return gtk_icon_view_set_text_column(arg0, arg1);
}

int Pure_gtk_icon_view_get_text_column(GtkIconView* arg0)
{
  return gtk_icon_view_get_text_column(arg0);
}

void Pure_gtk_icon_view_set_markup_column(GtkIconView* arg0, int arg1)
{
  return gtk_icon_view_set_markup_column(arg0, arg1);
}

int Pure_gtk_icon_view_get_markup_column(GtkIconView* arg0)
{
  return gtk_icon_view_get_markup_column(arg0);
}

void Pure_gtk_icon_view_set_pixbuf_column(GtkIconView* arg0, int arg1)
{
  return gtk_icon_view_set_pixbuf_column(arg0, arg1);
}

int Pure_gtk_icon_view_get_pixbuf_column(GtkIconView* arg0)
{
  return gtk_icon_view_get_pixbuf_column(arg0);
}

void Pure_gtk_icon_view_set_orientation(GtkIconView* arg0, unsigned int arg1)
{
  return gtk_icon_view_set_orientation(arg0, arg1);
}

unsigned int Pure_gtk_icon_view_get_orientation(GtkIconView* arg0)
{
  return gtk_icon_view_get_orientation(arg0);
}

void Pure_gtk_icon_view_set_columns(GtkIconView* arg0, int arg1)
{
  return gtk_icon_view_set_columns(arg0, arg1);
}

int Pure_gtk_icon_view_get_columns(GtkIconView* arg0)
{
  return gtk_icon_view_get_columns(arg0);
}

void Pure_gtk_icon_view_set_item_width(GtkIconView* arg0, int arg1)
{
  return gtk_icon_view_set_item_width(arg0, arg1);
}

int Pure_gtk_icon_view_get_item_width(GtkIconView* arg0)
{
  return gtk_icon_view_get_item_width(arg0);
}

void Pure_gtk_icon_view_set_spacing(GtkIconView* arg0, int arg1)
{
  return gtk_icon_view_set_spacing(arg0, arg1);
}

int Pure_gtk_icon_view_get_spacing(GtkIconView* arg0)
{
  return gtk_icon_view_get_spacing(arg0);
}

void Pure_gtk_icon_view_set_row_spacing(GtkIconView* arg0, int arg1)
{
  return gtk_icon_view_set_row_spacing(arg0, arg1);
}

int Pure_gtk_icon_view_get_row_spacing(GtkIconView* arg0)
{
  return gtk_icon_view_get_row_spacing(arg0);
}

void Pure_gtk_icon_view_set_column_spacing(GtkIconView* arg0, int arg1)
{
  return gtk_icon_view_set_column_spacing(arg0, arg1);
}

int Pure_gtk_icon_view_get_column_spacing(GtkIconView* arg0)
{
  return gtk_icon_view_get_column_spacing(arg0);
}

void Pure_gtk_icon_view_set_margin(GtkIconView* arg0, int arg1)
{
  return gtk_icon_view_set_margin(arg0, arg1);
}

int Pure_gtk_icon_view_get_margin(GtkIconView* arg0)
{
  return gtk_icon_view_get_margin(arg0);
}

GtkTreePath* Pure_gtk_icon_view_get_path_at_pos(GtkIconView* arg0, int arg1, int arg2)
{
  return gtk_icon_view_get_path_at_pos(arg0, arg1, arg2);
}

int Pure_gtk_icon_view_get_item_at_pos(GtkIconView* arg0, int arg1, int arg2, GtkTreePath** arg3, GtkCellRenderer** arg4)
{
  return gtk_icon_view_get_item_at_pos(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gtk_icon_view_get_visible_range(GtkIconView* arg0, GtkTreePath** arg1, GtkTreePath** arg2)
{
  return gtk_icon_view_get_visible_range(arg0, arg1, arg2);
}

void Pure_gtk_icon_view_selected_foreach(GtkIconView* arg0, void* arg1, void* arg2)
{
  return gtk_icon_view_selected_foreach(arg0, arg1, arg2);
}

void Pure_gtk_icon_view_set_selection_mode(GtkIconView* arg0, unsigned int arg1)
{
  return gtk_icon_view_set_selection_mode(arg0, arg1);
}

unsigned int Pure_gtk_icon_view_get_selection_mode(GtkIconView* arg0)
{
  return gtk_icon_view_get_selection_mode(arg0);
}

void Pure_gtk_icon_view_select_path(GtkIconView* arg0, GtkTreePath* arg1)
{
  return gtk_icon_view_select_path(arg0, arg1);
}

void Pure_gtk_icon_view_unselect_path(GtkIconView* arg0, GtkTreePath* arg1)
{
  return gtk_icon_view_unselect_path(arg0, arg1);
}

int Pure_gtk_icon_view_path_is_selected(GtkIconView* arg0, GtkTreePath* arg1)
{
  return gtk_icon_view_path_is_selected(arg0, arg1);
}

GList* Pure_gtk_icon_view_get_selected_items(GtkIconView* arg0)
{
  return gtk_icon_view_get_selected_items(arg0);
}

void Pure_gtk_icon_view_select_all(GtkIconView* arg0)
{
  return gtk_icon_view_select_all(arg0);
}

void Pure_gtk_icon_view_unselect_all(GtkIconView* arg0)
{
  return gtk_icon_view_unselect_all(arg0);
}

void Pure_gtk_icon_view_item_activated(GtkIconView* arg0, GtkTreePath* arg1)
{
  return gtk_icon_view_item_activated(arg0, arg1);
}

void Pure_gtk_icon_view_set_cursor(GtkIconView* arg0, GtkTreePath* arg1, GtkCellRenderer* arg2, int arg3)
{
  return gtk_icon_view_set_cursor(arg0, arg1, arg2, arg3);
}

int Pure_gtk_icon_view_get_cursor(GtkIconView* arg0, GtkTreePath** arg1, GtkCellRenderer** arg2)
{
  return gtk_icon_view_get_cursor(arg0, arg1, arg2);
}

void Pure_gtk_icon_view_scroll_to_path(GtkIconView* arg0, GtkTreePath* arg1, int arg2, float arg3, float arg4)
{
  return gtk_icon_view_scroll_to_path(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_icon_view_enable_model_drag_source(GtkIconView* arg0, unsigned int arg1, GtkTargetEntry const* arg2, int arg3, unsigned int arg4)
{
  return gtk_icon_view_enable_model_drag_source(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_icon_view_enable_model_drag_dest(GtkIconView* arg0, GtkTargetEntry const* arg1, int arg2, unsigned int arg3)
{
  return gtk_icon_view_enable_model_drag_dest(arg0, arg1, arg2, arg3);
}

void Pure_gtk_icon_view_unset_model_drag_source(GtkIconView* arg0)
{
  return gtk_icon_view_unset_model_drag_source(arg0);
}

void Pure_gtk_icon_view_unset_model_drag_dest(GtkIconView* arg0)
{
  return gtk_icon_view_unset_model_drag_dest(arg0);
}

void Pure_gtk_icon_view_set_reorderable(GtkIconView* arg0, int arg1)
{
  return gtk_icon_view_set_reorderable(arg0, arg1);
}

int Pure_gtk_icon_view_get_reorderable(GtkIconView* arg0)
{
  return gtk_icon_view_get_reorderable(arg0);
}

void Pure_gtk_icon_view_set_drag_dest_item(GtkIconView* arg0, GtkTreePath* arg1, unsigned int arg2)
{
  return gtk_icon_view_set_drag_dest_item(arg0, arg1, arg2);
}

void Pure_gtk_icon_view_get_drag_dest_item(GtkIconView* arg0, GtkTreePath** arg1, unsigned int* arg2)
{
  return gtk_icon_view_get_drag_dest_item(arg0, arg1, arg2);
}

int Pure_gtk_icon_view_get_dest_item_at_pos(GtkIconView* arg0, int arg1, int arg2, GtkTreePath** arg3, unsigned int* arg4)
{
  return gtk_icon_view_get_dest_item_at_pos(arg0, arg1, arg2, arg3, arg4);
}

GdkPixmap* Pure_gtk_icon_view_create_drag_icon(GtkIconView* arg0, GtkTreePath* arg1)
{
  return gtk_icon_view_create_drag_icon(arg0, arg1);
}

void Pure_gtk_icon_view_convert_widget_to_bin_window_coords(GtkIconView* arg0, int arg1, int arg2, int* arg3, int* arg4)
{
  return gtk_icon_view_convert_widget_to_bin_window_coords(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_icon_view_set_tooltip_item(GtkIconView* arg0, GtkTooltip* arg1, GtkTreePath* arg2)
{
  return gtk_icon_view_set_tooltip_item(arg0, arg1, arg2);
}

void Pure_gtk_icon_view_set_tooltip_cell(GtkIconView* arg0, GtkTooltip* arg1, GtkTreePath* arg2, GtkCellRenderer* arg3)
{
  return gtk_icon_view_set_tooltip_cell(arg0, arg1, arg2, arg3);
}

int Pure_gtk_icon_view_get_tooltip_context(GtkIconView* arg0, int* arg1, int* arg2, int arg3, GtkTreeModel** arg4, GtkTreePath** arg5, GtkTreeIter* arg6)
{
  return gtk_icon_view_get_tooltip_context(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_icon_view_set_tooltip_column(GtkIconView* arg0, int arg1)
{
  return gtk_icon_view_set_tooltip_column(arg0, arg1);
}

int Pure_gtk_icon_view_get_tooltip_column(GtkIconView* arg0)
{
  return gtk_icon_view_get_tooltip_column(arg0);
}

unsigned long Pure_gtk_image_menu_item_get_type()
{
  return gtk_image_menu_item_get_type();
}

GtkWidget* Pure_gtk_image_menu_item_new()
{
  return gtk_image_menu_item_new();
}

GtkWidget* Pure_gtk_image_menu_item_new_with_label(char const* arg0)
{
  return gtk_image_menu_item_new_with_label(arg0);
}

GtkWidget* Pure_gtk_image_menu_item_new_with_mnemonic(char const* arg0)
{
  return gtk_image_menu_item_new_with_mnemonic(arg0);
}

GtkWidget* Pure_gtk_image_menu_item_new_from_stock(char const* arg0, GtkAccelGroup* arg1)
{
  return gtk_image_menu_item_new_from_stock(arg0, arg1);
}

void Pure_gtk_image_menu_item_set_image(GtkImageMenuItem* arg0, GtkWidget* arg1)
{
  return gtk_image_menu_item_set_image(arg0, arg1);
}

GtkWidget* Pure_gtk_image_menu_item_get_image(GtkImageMenuItem* arg0)
{
  return gtk_image_menu_item_get_image(arg0);
}

unsigned long Pure_gtk_im_context_simple_get_type()
{
  return gtk_im_context_simple_get_type();
}

GtkIMContext* Pure_gtk_im_context_simple_new()
{
  return gtk_im_context_simple_new();
}

void Pure_gtk_im_context_simple_add_table(GtkIMContextSimple* arg0, unsigned short* arg1, int arg2, int arg3)
{
  return gtk_im_context_simple_add_table(arg0, arg1, arg2, arg3);
}

unsigned long Pure_gtk_im_multicontext_get_type()
{
  return gtk_im_multicontext_get_type();
}

GtkIMContext* Pure_gtk_im_multicontext_new()
{
  return gtk_im_multicontext_new();
}

void Pure_gtk_im_multicontext_append_menuitems(GtkIMMulticontext* arg0, GtkMenuShell* arg1)
{
  return gtk_im_multicontext_append_menuitems(arg0, arg1);
}

unsigned long Pure_gtk_input_dialog_get_type()
{
  return gtk_input_dialog_get_type();
}

GtkWidget* Pure_gtk_input_dialog_new()
{
  return gtk_input_dialog_new();
}

unsigned long Pure_gtk_invisible_get_type()
{
  return gtk_invisible_get_type();
}

GtkWidget* Pure_gtk_invisible_new()
{
  return gtk_invisible_new();
}

GtkWidget* Pure_gtk_invisible_new_for_screen(GdkScreen* arg0)
{
  return gtk_invisible_new_for_screen(arg0);
}

void Pure_gtk_invisible_set_screen(GtkInvisible* arg0, GdkScreen* arg1)
{
  return gtk_invisible_set_screen(arg0, arg1);
}

GdkScreen* Pure_gtk_invisible_get_screen(GtkInvisible* arg0)
{
  return gtk_invisible_get_screen(arg0);
}

unsigned long Pure_gtk_layout_get_type()
{
  return gtk_layout_get_type();
}

GtkWidget* Pure_gtk_layout_new(GtkAdjustment* arg0, GtkAdjustment* arg1)
{
  return gtk_layout_new(arg0, arg1);
}

GdkWindow* Pure_gtk_layout_get_bin_window(GtkLayout* arg0)
{
  return gtk_layout_get_bin_window(arg0);
}

void Pure_gtk_layout_put(GtkLayout* arg0, GtkWidget* arg1, int arg2, int arg3)
{
  return gtk_layout_put(arg0, arg1, arg2, arg3);
}

void Pure_gtk_layout_move(GtkLayout* arg0, GtkWidget* arg1, int arg2, int arg3)
{
  return gtk_layout_move(arg0, arg1, arg2, arg3);
}

void Pure_gtk_layout_set_size(GtkLayout* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_layout_set_size(arg0, arg1, arg2);
}

void Pure_gtk_layout_get_size(GtkLayout* arg0, unsigned int* arg1, unsigned int* arg2)
{
  return gtk_layout_get_size(arg0, arg1, arg2);
}

GtkAdjustment* Pure_gtk_layout_get_hadjustment(GtkLayout* arg0)
{
  return gtk_layout_get_hadjustment(arg0);
}

GtkAdjustment* Pure_gtk_layout_get_vadjustment(GtkLayout* arg0)
{
  return gtk_layout_get_vadjustment(arg0);
}

void Pure_gtk_layout_set_hadjustment(GtkLayout* arg0, GtkAdjustment* arg1)
{
  return gtk_layout_set_hadjustment(arg0, arg1);
}

void Pure_gtk_layout_set_vadjustment(GtkLayout* arg0, GtkAdjustment* arg1)
{
  return gtk_layout_set_vadjustment(arg0, arg1);
}

void Pure_gtk_layout_freeze(GtkLayout* arg0)
{
  return gtk_layout_freeze(arg0);
}

void Pure_gtk_layout_thaw(GtkLayout* arg0)
{
  return gtk_layout_thaw(arg0);
}

unsigned long Pure_gtk_link_button_get_type()
{
  return gtk_link_button_get_type();
}

GtkWidget* Pure_gtk_link_button_new(char const* arg0)
{
  return gtk_link_button_new(arg0);
}

GtkWidget* Pure_gtk_link_button_new_with_label(char const* arg0, char const* arg1)
{
  return gtk_link_button_new_with_label(arg0, arg1);
}

char const* Pure_gtk_link_button_get_uri(GtkLinkButton* arg0)
{
  return gtk_link_button_get_uri(arg0);
}

void Pure_gtk_link_button_set_uri(GtkLinkButton* arg0, char const* arg1)
{
  return gtk_link_button_set_uri(arg0, arg1);
}

void* Pure_gtk_link_button_set_uri_hook(void* arg0, void* arg1, void* arg2)
{
  return gtk_link_button_set_uri_hook(arg0, arg1, arg2);
}

int Pure_gtk_link_button_get_visited(GtkLinkButton* arg0)
{
  return gtk_link_button_get_visited(arg0);
}

void Pure_gtk_link_button_set_visited(GtkLinkButton* arg0, int arg1)
{
  return gtk_link_button_set_visited(arg0, arg1);
}

char const* Pure_gtk_check_version(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_check_version(arg0, arg1, arg2);
}

int Pure_gtk_parse_args(int* arg0, char*** arg1)
{
  return gtk_parse_args(arg0, arg1);
}

void Pure_gtk_init(int* arg0, char*** arg1)
{
  return gtk_init(arg0, arg1);
}

int Pure_gtk_init_check(int* arg0, char*** arg1)
{
  return gtk_init_check(arg0, arg1);
}

int Pure_gtk_init_with_args(int* arg0, char*** arg1, char* arg2, GOptionEntry* arg3, char* arg4, GError** arg5)
{
  return gtk_init_with_args(arg0, arg1, arg2, arg3, arg4, arg5);
}

GOptionGroup* Pure_gtk_get_option_group(int arg0)
{
  return gtk_get_option_group(arg0);
}

void Pure_gtk_exit(int arg0)
{
  return gtk_exit(arg0);
}

void Pure_gtk_disable_setlocale()
{
  return gtk_disable_setlocale();
}

char* Pure_gtk_set_locale()
{
  return gtk_set_locale();
}

PangoLanguage* Pure_gtk_get_default_language()
{
  return gtk_get_default_language();
}

int Pure_gtk_events_pending()
{
  return gtk_events_pending();
}

void Pure_gtk_main_do_event(GdkEvent* arg0)
{
  return gtk_main_do_event(arg0);
}

void Pure_gtk_main()
{
  return gtk_main();
}

unsigned int Pure_gtk_main_level()
{
  return gtk_main_level();
}

void Pure_gtk_main_quit()
{
  return gtk_main_quit();
}

int Pure_gtk_main_iteration()
{
  return gtk_main_iteration();
}

int Pure_gtk_main_iteration_do(int arg0)
{
  return gtk_main_iteration_do(arg0);
}

int Pure_gtk_true()
{
  return gtk_true();
}

int Pure_gtk_false()
{
  return gtk_false();
}

void Pure_gtk_grab_add(GtkWidget* arg0)
{
  return gtk_grab_add(arg0);
}

GtkWidget* Pure_gtk_grab_get_current()
{
  return gtk_grab_get_current();
}

void Pure_gtk_grab_remove(GtkWidget* arg0)
{
  return gtk_grab_remove(arg0);
}

void Pure_gtk_init_add(void* arg0, void* arg1)
{
  return gtk_init_add(arg0, arg1);
}

void Pure_gtk_quit_add_destroy(unsigned int arg0, GtkObject* arg1)
{
  return gtk_quit_add_destroy(arg0, arg1);
}

unsigned int Pure_gtk_quit_add(unsigned int arg0, void* arg1, void* arg2)
{
  return gtk_quit_add(arg0, arg1, arg2);
}

unsigned int Pure_gtk_quit_add_full(unsigned int arg0, void* arg1, void* arg2, void* arg3, void* arg4)
{
  return gtk_quit_add_full(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_quit_remove(unsigned int arg0)
{
  return gtk_quit_remove(arg0);
}

void Pure_gtk_quit_remove_by_data(void* arg0)
{
  return gtk_quit_remove_by_data(arg0);
}

unsigned int Pure_gtk_timeout_add(unsigned int arg0, void* arg1, void* arg2)
{
  return gtk_timeout_add(arg0, arg1, arg2);
}

unsigned int Pure_gtk_timeout_add_full(unsigned int arg0, void* arg1, void* arg2, void* arg3, void* arg4)
{
  return gtk_timeout_add_full(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_timeout_remove(unsigned int arg0)
{
  return gtk_timeout_remove(arg0);
}

unsigned int Pure_gtk_idle_add(void* arg0, void* arg1)
{
  return gtk_idle_add(arg0, arg1);
}

unsigned int Pure_gtk_idle_add_priority(int arg0, void* arg1, void* arg2)
{
  return gtk_idle_add_priority(arg0, arg1, arg2);
}

unsigned int Pure_gtk_idle_add_full(int arg0, void* arg1, void* arg2, void* arg3, void* arg4)
{
  return gtk_idle_add_full(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_idle_remove(unsigned int arg0)
{
  return gtk_idle_remove(arg0);
}

void Pure_gtk_idle_remove_by_data(void* arg0)
{
  return gtk_idle_remove_by_data(arg0);
}

unsigned int Pure_gtk_input_add_full(int arg0, unsigned int arg1, void* arg2, void* arg3, void* arg4, void* arg5)
{
  return gtk_input_add_full(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_input_remove(unsigned int arg0)
{
  return gtk_input_remove(arg0);
}

unsigned int Pure_gtk_key_snooper_install(void* arg0, void* arg1)
{
  return gtk_key_snooper_install(arg0, arg1);
}

void Pure_gtk_key_snooper_remove(unsigned int arg0)
{
  return gtk_key_snooper_remove(arg0);
}

GdkEvent* Pure_gtk_get_current_event()
{
  return gtk_get_current_event();
}

unsigned int Pure_gtk_get_current_event_time()
{
  return gtk_get_current_event_time();
}

int Pure_gtk_get_current_event_state(unsigned int* arg0)
{
  return gtk_get_current_event_state(arg0);
}

GtkWidget* Pure_gtk_get_event_widget(GdkEvent* arg0)
{
  return gtk_get_event_widget(arg0);
}

void Pure_gtk_propagate_event(GtkWidget* arg0, GdkEvent* arg1)
{
  return gtk_propagate_event(arg0, arg1);
}

unsigned long Pure_gtk_menu_bar_get_type()
{
  return gtk_menu_bar_get_type();
}

GtkWidget* Pure_gtk_menu_bar_new()
{
  return gtk_menu_bar_new();
}

unsigned int Pure_gtk_menu_bar_get_pack_direction(GtkMenuBar* arg0)
{
  return gtk_menu_bar_get_pack_direction(arg0);
}

void Pure_gtk_menu_bar_set_pack_direction(GtkMenuBar* arg0, unsigned int arg1)
{
  return gtk_menu_bar_set_pack_direction(arg0, arg1);
}

unsigned int Pure_gtk_menu_bar_get_child_pack_direction(GtkMenuBar* arg0)
{
  return gtk_menu_bar_get_child_pack_direction(arg0);
}

void Pure_gtk_menu_bar_set_child_pack_direction(GtkMenuBar* arg0, unsigned int arg1)
{
  return gtk_menu_bar_set_child_pack_direction(arg0, arg1);
}

unsigned long Pure_gtk_tooltips_get_type()
{
  return gtk_tooltips_get_type();
}

GtkTooltips* Pure_gtk_tooltips_new()
{
  return gtk_tooltips_new();
}

void Pure_gtk_tooltips_enable(GtkTooltips* arg0)
{
  return gtk_tooltips_enable(arg0);
}

void Pure_gtk_tooltips_disable(GtkTooltips* arg0)
{
  return gtk_tooltips_disable(arg0);
}

void Pure_gtk_tooltips_set_delay(GtkTooltips* arg0, unsigned int arg1)
{
  return gtk_tooltips_set_delay(arg0, arg1);
}

void Pure_gtk_tooltips_set_tip(GtkTooltips* arg0, GtkWidget* arg1, char const* arg2, char const* arg3)
{
  return gtk_tooltips_set_tip(arg0, arg1, arg2, arg3);
}

GtkTooltipsData* Pure_gtk_tooltips_data_get(GtkWidget* arg0)
{
  return gtk_tooltips_data_get(arg0);
}

void Pure_gtk_tooltips_force_window(GtkTooltips* arg0)
{
  return gtk_tooltips_force_window(arg0);
}

int Pure_gtk_tooltips_get_info_from_tip_window(GtkWindow* arg0, GtkTooltips** arg1, GtkWidget** arg2)
{
  return gtk_tooltips_get_info_from_tip_window(arg0, arg1, arg2);
}

unsigned long Pure_gtk_tool_item_get_type()
{
  return gtk_tool_item_get_type();
}

GtkToolItem* Pure_gtk_tool_item_new()
{
  return gtk_tool_item_new();
}

void Pure_gtk_tool_item_set_homogeneous(GtkToolItem* arg0, int arg1)
{
  return gtk_tool_item_set_homogeneous(arg0, arg1);
}

int Pure_gtk_tool_item_get_homogeneous(GtkToolItem* arg0)
{
  return gtk_tool_item_get_homogeneous(arg0);
}

void Pure_gtk_tool_item_set_expand(GtkToolItem* arg0, int arg1)
{
  return gtk_tool_item_set_expand(arg0, arg1);
}

int Pure_gtk_tool_item_get_expand(GtkToolItem* arg0)
{
  return gtk_tool_item_get_expand(arg0);
}

void Pure_gtk_tool_item_set_tooltip(GtkToolItem* arg0, GtkTooltips* arg1, char const* arg2, char const* arg3)
{
  return gtk_tool_item_set_tooltip(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tool_item_set_tooltip_text(GtkToolItem* arg0, char const* arg1)
{
  return gtk_tool_item_set_tooltip_text(arg0, arg1);
}

void Pure_gtk_tool_item_set_tooltip_markup(GtkToolItem* arg0, char const* arg1)
{
  return gtk_tool_item_set_tooltip_markup(arg0, arg1);
}

void Pure_gtk_tool_item_set_use_drag_window(GtkToolItem* arg0, int arg1)
{
  return gtk_tool_item_set_use_drag_window(arg0, arg1);
}

int Pure_gtk_tool_item_get_use_drag_window(GtkToolItem* arg0)
{
  return gtk_tool_item_get_use_drag_window(arg0);
}

void Pure_gtk_tool_item_set_visible_horizontal(GtkToolItem* arg0, int arg1)
{
  return gtk_tool_item_set_visible_horizontal(arg0, arg1);
}

int Pure_gtk_tool_item_get_visible_horizontal(GtkToolItem* arg0)
{
  return gtk_tool_item_get_visible_horizontal(arg0);
}

void Pure_gtk_tool_item_set_visible_vertical(GtkToolItem* arg0, int arg1)
{
  return gtk_tool_item_set_visible_vertical(arg0, arg1);
}

int Pure_gtk_tool_item_get_visible_vertical(GtkToolItem* arg0)
{
  return gtk_tool_item_get_visible_vertical(arg0);
}

int Pure_gtk_tool_item_get_is_important(GtkToolItem* arg0)
{
  return gtk_tool_item_get_is_important(arg0);
}

void Pure_gtk_tool_item_set_is_important(GtkToolItem* arg0, int arg1)
{
  return gtk_tool_item_set_is_important(arg0, arg1);
}

unsigned int Pure_gtk_tool_item_get_icon_size(GtkToolItem* arg0)
{
  return gtk_tool_item_get_icon_size(arg0);
}

unsigned int Pure_gtk_tool_item_get_orientation(GtkToolItem* arg0)
{
  return gtk_tool_item_get_orientation(arg0);
}

unsigned int Pure_gtk_tool_item_get_toolbar_style(GtkToolItem* arg0)
{
  return gtk_tool_item_get_toolbar_style(arg0);
}

unsigned int Pure_gtk_tool_item_get_relief_style(GtkToolItem* arg0)
{
  return gtk_tool_item_get_relief_style(arg0);
}

GtkWidget* Pure_gtk_tool_item_retrieve_proxy_menu_item(GtkToolItem* arg0)
{
  return gtk_tool_item_retrieve_proxy_menu_item(arg0);
}

GtkWidget* Pure_gtk_tool_item_get_proxy_menu_item(GtkToolItem* arg0, char const* arg1)
{
  return gtk_tool_item_get_proxy_menu_item(arg0, arg1);
}

void Pure_gtk_tool_item_set_proxy_menu_item(GtkToolItem* arg0, char const* arg1, GtkWidget* arg2)
{
  return gtk_tool_item_set_proxy_menu_item(arg0, arg1, arg2);
}

void Pure_gtk_tool_item_rebuild_menu(GtkToolItem* arg0)
{
  return gtk_tool_item_rebuild_menu(arg0);
}

void Pure_gtk_tool_item_toolbar_reconfigured(GtkToolItem* arg0)
{
  return gtk_tool_item_toolbar_reconfigured(arg0);
}

unsigned long Pure_gtk_tool_button_get_type()
{
  return gtk_tool_button_get_type();
}

GtkToolItem* Pure_gtk_tool_button_new(GtkWidget* arg0, char const* arg1)
{
  return gtk_tool_button_new(arg0, arg1);
}

GtkToolItem* Pure_gtk_tool_button_new_from_stock(char const* arg0)
{
  return gtk_tool_button_new_from_stock(arg0);
}

void Pure_gtk_tool_button_set_label(GtkToolButton* arg0, char const* arg1)
{
  return gtk_tool_button_set_label(arg0, arg1);
}

char const* Pure_gtk_tool_button_get_label(GtkToolButton* arg0)
{
  return gtk_tool_button_get_label(arg0);
}

void Pure_gtk_tool_button_set_use_underline(GtkToolButton* arg0, int arg1)
{
  return gtk_tool_button_set_use_underline(arg0, arg1);
}

int Pure_gtk_tool_button_get_use_underline(GtkToolButton* arg0)
{
  return gtk_tool_button_get_use_underline(arg0);
}

void Pure_gtk_tool_button_set_stock_id(GtkToolButton* arg0, char const* arg1)
{
  return gtk_tool_button_set_stock_id(arg0, arg1);
}

char const* Pure_gtk_tool_button_get_stock_id(GtkToolButton* arg0)
{
  return gtk_tool_button_get_stock_id(arg0);
}

void Pure_gtk_tool_button_set_icon_name(GtkToolButton* arg0, char const* arg1)
{
  return gtk_tool_button_set_icon_name(arg0, arg1);
}

char const* Pure_gtk_tool_button_get_icon_name(GtkToolButton* arg0)
{
  return gtk_tool_button_get_icon_name(arg0);
}

void Pure_gtk_tool_button_set_icon_widget(GtkToolButton* arg0, GtkWidget* arg1)
{
  return gtk_tool_button_set_icon_widget(arg0, arg1);
}

GtkWidget* Pure_gtk_tool_button_get_icon_widget(GtkToolButton* arg0)
{
  return gtk_tool_button_get_icon_widget(arg0);
}

void Pure_gtk_tool_button_set_label_widget(GtkToolButton* arg0, GtkWidget* arg1)
{
  return gtk_tool_button_set_label_widget(arg0, arg1);
}

GtkWidget* Pure_gtk_tool_button_get_label_widget(GtkToolButton* arg0)
{
  return gtk_tool_button_get_label_widget(arg0);
}

unsigned long Pure_gtk_menu_tool_button_get_type()
{
  return gtk_menu_tool_button_get_type();
}

GtkToolItem* Pure_gtk_menu_tool_button_new(GtkWidget* arg0, char const* arg1)
{
  return gtk_menu_tool_button_new(arg0, arg1);
}

GtkToolItem* Pure_gtk_menu_tool_button_new_from_stock(char const* arg0)
{
  return gtk_menu_tool_button_new_from_stock(arg0);
}

void Pure_gtk_menu_tool_button_set_menu(GtkMenuToolButton* arg0, GtkWidget* arg1)
{
  return gtk_menu_tool_button_set_menu(arg0, arg1);
}

GtkWidget* Pure_gtk_menu_tool_button_get_menu(GtkMenuToolButton* arg0)
{
  return gtk_menu_tool_button_get_menu(arg0);
}

void Pure_gtk_menu_tool_button_set_arrow_tooltip(GtkMenuToolButton* arg0, GtkTooltips* arg1, char const* arg2, char const* arg3)
{
  return gtk_menu_tool_button_set_arrow_tooltip(arg0, arg1, arg2, arg3);
}

void Pure_gtk_menu_tool_button_set_arrow_tooltip_text(GtkMenuToolButton* arg0, char const* arg1)
{
  return gtk_menu_tool_button_set_arrow_tooltip_text(arg0, arg1);
}

void Pure_gtk_menu_tool_button_set_arrow_tooltip_markup(GtkMenuToolButton* arg0, char const* arg1)
{
  return gtk_menu_tool_button_set_arrow_tooltip_markup(arg0, arg1);
}

unsigned long Pure_gtk_message_dialog_get_type()
{
  return gtk_message_dialog_get_type();
}

GtkWidget* Pure_gtk_message_dialog_new(GtkWindow* arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, char const* arg4)
{
  return gtk_message_dialog_new(arg0, arg1, arg2, arg3, arg4);
}

GtkWidget* Pure_gtk_message_dialog_new_with_markup(GtkWindow* arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, char const* arg4)
{
  return gtk_message_dialog_new_with_markup(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_message_dialog_set_image(GtkMessageDialog* arg0, GtkWidget* arg1)
{
  return gtk_message_dialog_set_image(arg0, arg1);
}

GtkWidget* Pure_gtk_message_dialog_get_image(GtkMessageDialog* arg0)
{
  return gtk_message_dialog_get_image(arg0);
}

void Pure_gtk_message_dialog_set_markup(GtkMessageDialog* arg0, char const* arg1)
{
  return gtk_message_dialog_set_markup(arg0, arg1);
}

void Pure_gtk_message_dialog_format_secondary_text(GtkMessageDialog* arg0, char const* arg1)
{
  return gtk_message_dialog_format_secondary_text(arg0, arg1);
}

void Pure_gtk_message_dialog_format_secondary_markup(GtkMessageDialog* arg0, char const* arg1)
{
  return gtk_message_dialog_format_secondary_markup(arg0, arg1);
}

unsigned long Pure_gtk_mount_operation_get_type()
{
  return gtk_mount_operation_get_type();
}

GMountOperation* Pure_gtk_mount_operation_new(GtkWindow* arg0)
{
  return gtk_mount_operation_new(arg0);
}

int Pure_gtk_mount_operation_is_showing(GtkMountOperation* arg0)
{
  return gtk_mount_operation_is_showing(arg0);
}

void Pure_gtk_mount_operation_set_parent(GtkMountOperation* arg0, GtkWindow* arg1)
{
  return gtk_mount_operation_set_parent(arg0, arg1);
}

GtkWindow* Pure_gtk_mount_operation_get_parent(GtkMountOperation* arg0)
{
  return gtk_mount_operation_get_parent(arg0);
}

void Pure_gtk_mount_operation_set_screen(GtkMountOperation* arg0, GdkScreen* arg1)
{
  return gtk_mount_operation_set_screen(arg0, arg1);
}

GdkScreen* Pure_gtk_mount_operation_get_screen(GtkMountOperation* arg0)
{
  return gtk_mount_operation_get_screen(arg0);
}

unsigned long Pure_gtk_notebook_get_type()
{
  return gtk_notebook_get_type();
}

GtkWidget* Pure_gtk_notebook_new()
{
  return gtk_notebook_new();
}

int Pure_gtk_notebook_append_page(GtkNotebook* arg0, GtkWidget* arg1, GtkWidget* arg2)
{
  return gtk_notebook_append_page(arg0, arg1, arg2);
}

int Pure_gtk_notebook_append_page_menu(GtkNotebook* arg0, GtkWidget* arg1, GtkWidget* arg2, GtkWidget* arg3)
{
  return gtk_notebook_append_page_menu(arg0, arg1, arg2, arg3);
}

int Pure_gtk_notebook_prepend_page(GtkNotebook* arg0, GtkWidget* arg1, GtkWidget* arg2)
{
  return gtk_notebook_prepend_page(arg0, arg1, arg2);
}

int Pure_gtk_notebook_prepend_page_menu(GtkNotebook* arg0, GtkWidget* arg1, GtkWidget* arg2, GtkWidget* arg3)
{
  return gtk_notebook_prepend_page_menu(arg0, arg1, arg2, arg3);
}

int Pure_gtk_notebook_insert_page(GtkNotebook* arg0, GtkWidget* arg1, GtkWidget* arg2, int arg3)
{
  return gtk_notebook_insert_page(arg0, arg1, arg2, arg3);
}

int Pure_gtk_notebook_insert_page_menu(GtkNotebook* arg0, GtkWidget* arg1, GtkWidget* arg2, GtkWidget* arg3, int arg4)
{
  return gtk_notebook_insert_page_menu(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_notebook_remove_page(GtkNotebook* arg0, int arg1)
{
  return gtk_notebook_remove_page(arg0, arg1);
}

void Pure_gtk_notebook_set_window_creation_hook(void* arg0, void* arg1, void* arg2)
{
  return gtk_notebook_set_window_creation_hook(arg0, arg1, arg2);
}

void Pure_gtk_notebook_set_group_id(GtkNotebook* arg0, int arg1)
{
  return gtk_notebook_set_group_id(arg0, arg1);
}

int Pure_gtk_notebook_get_group_id(GtkNotebook* arg0)
{
  return gtk_notebook_get_group_id(arg0);
}

void Pure_gtk_notebook_set_group(GtkNotebook* arg0, void* arg1)
{
  return gtk_notebook_set_group(arg0, arg1);
}

void* Pure_gtk_notebook_get_group(GtkNotebook* arg0)
{
  return gtk_notebook_get_group(arg0);
}

int Pure_gtk_notebook_get_current_page(GtkNotebook* arg0)
{
  return gtk_notebook_get_current_page(arg0);
}

GtkWidget* Pure_gtk_notebook_get_nth_page(GtkNotebook* arg0, int arg1)
{
  return gtk_notebook_get_nth_page(arg0, arg1);
}

int Pure_gtk_notebook_get_n_pages(GtkNotebook* arg0)
{
  return gtk_notebook_get_n_pages(arg0);
}

int Pure_gtk_notebook_page_num(GtkNotebook* arg0, GtkWidget* arg1)
{
  return gtk_notebook_page_num(arg0, arg1);
}

void Pure_gtk_notebook_set_current_page(GtkNotebook* arg0, int arg1)
{
  return gtk_notebook_set_current_page(arg0, arg1);
}

void Pure_gtk_notebook_next_page(GtkNotebook* arg0)
{
  return gtk_notebook_next_page(arg0);
}

void Pure_gtk_notebook_prev_page(GtkNotebook* arg0)
{
  return gtk_notebook_prev_page(arg0);
}

void Pure_gtk_notebook_set_show_border(GtkNotebook* arg0, int arg1)
{
  return gtk_notebook_set_show_border(arg0, arg1);
}

int Pure_gtk_notebook_get_show_border(GtkNotebook* arg0)
{
  return gtk_notebook_get_show_border(arg0);
}

void Pure_gtk_notebook_set_show_tabs(GtkNotebook* arg0, int arg1)
{
  return gtk_notebook_set_show_tabs(arg0, arg1);
}

int Pure_gtk_notebook_get_show_tabs(GtkNotebook* arg0)
{
  return gtk_notebook_get_show_tabs(arg0);
}

void Pure_gtk_notebook_set_tab_pos(GtkNotebook* arg0, unsigned int arg1)
{
  return gtk_notebook_set_tab_pos(arg0, arg1);
}

unsigned int Pure_gtk_notebook_get_tab_pos(GtkNotebook* arg0)
{
  return gtk_notebook_get_tab_pos(arg0);
}

void Pure_gtk_notebook_set_homogeneous_tabs(GtkNotebook* arg0, int arg1)
{
  return gtk_notebook_set_homogeneous_tabs(arg0, arg1);
}

void Pure_gtk_notebook_set_tab_border(GtkNotebook* arg0, unsigned int arg1)
{
  return gtk_notebook_set_tab_border(arg0, arg1);
}

void Pure_gtk_notebook_set_tab_hborder(GtkNotebook* arg0, unsigned int arg1)
{
  return gtk_notebook_set_tab_hborder(arg0, arg1);
}

void Pure_gtk_notebook_set_tab_vborder(GtkNotebook* arg0, unsigned int arg1)
{
  return gtk_notebook_set_tab_vborder(arg0, arg1);
}

void Pure_gtk_notebook_set_scrollable(GtkNotebook* arg0, int arg1)
{
  return gtk_notebook_set_scrollable(arg0, arg1);
}

int Pure_gtk_notebook_get_scrollable(GtkNotebook* arg0)
{
  return gtk_notebook_get_scrollable(arg0);
}

void Pure_gtk_notebook_popup_enable(GtkNotebook* arg0)
{
  return gtk_notebook_popup_enable(arg0);
}

void Pure_gtk_notebook_popup_disable(GtkNotebook* arg0)
{
  return gtk_notebook_popup_disable(arg0);
}

GtkWidget* Pure_gtk_notebook_get_tab_label(GtkNotebook* arg0, GtkWidget* arg1)
{
  return gtk_notebook_get_tab_label(arg0, arg1);
}

void Pure_gtk_notebook_set_tab_label(GtkNotebook* arg0, GtkWidget* arg1, GtkWidget* arg2)
{
  return gtk_notebook_set_tab_label(arg0, arg1, arg2);
}

void Pure_gtk_notebook_set_tab_label_text(GtkNotebook* arg0, GtkWidget* arg1, char const* arg2)
{
  return gtk_notebook_set_tab_label_text(arg0, arg1, arg2);
}

char const* Pure_gtk_notebook_get_tab_label_text(GtkNotebook* arg0, GtkWidget* arg1)
{
  return gtk_notebook_get_tab_label_text(arg0, arg1);
}

GtkWidget* Pure_gtk_notebook_get_menu_label(GtkNotebook* arg0, GtkWidget* arg1)
{
  return gtk_notebook_get_menu_label(arg0, arg1);
}

void Pure_gtk_notebook_set_menu_label(GtkNotebook* arg0, GtkWidget* arg1, GtkWidget* arg2)
{
  return gtk_notebook_set_menu_label(arg0, arg1, arg2);
}

void Pure_gtk_notebook_set_menu_label_text(GtkNotebook* arg0, GtkWidget* arg1, char const* arg2)
{
  return gtk_notebook_set_menu_label_text(arg0, arg1, arg2);
}

char const* Pure_gtk_notebook_get_menu_label_text(GtkNotebook* arg0, GtkWidget* arg1)
{
  return gtk_notebook_get_menu_label_text(arg0, arg1);
}

void Pure_gtk_notebook_query_tab_label_packing(GtkNotebook* arg0, GtkWidget* arg1, int* arg2, int* arg3, unsigned int* arg4)
{
  return gtk_notebook_query_tab_label_packing(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_notebook_set_tab_label_packing(GtkNotebook* arg0, GtkWidget* arg1, int arg2, int arg3, unsigned int arg4)
{
  return gtk_notebook_set_tab_label_packing(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_notebook_reorder_child(GtkNotebook* arg0, GtkWidget* arg1, int arg2)
{
  return gtk_notebook_reorder_child(arg0, arg1, arg2);
}

int Pure_gtk_notebook_get_tab_reorderable(GtkNotebook* arg0, GtkWidget* arg1)
{
  return gtk_notebook_get_tab_reorderable(arg0, arg1);
}

void Pure_gtk_notebook_set_tab_reorderable(GtkNotebook* arg0, GtkWidget* arg1, int arg2)
{
  return gtk_notebook_set_tab_reorderable(arg0, arg1, arg2);
}

int Pure_gtk_notebook_get_tab_detachable(GtkNotebook* arg0, GtkWidget* arg1)
{
  return gtk_notebook_get_tab_detachable(arg0, arg1);
}

void Pure_gtk_notebook_set_tab_detachable(GtkNotebook* arg0, GtkWidget* arg1, int arg2)
{
  return gtk_notebook_set_tab_detachable(arg0, arg1, arg2);
}

unsigned long Pure_gtk_paper_size_get_type()
{
  return gtk_paper_size_get_type();
}

GtkPaperSize* Pure_gtk_paper_size_new(char const* arg0)
{
  return gtk_paper_size_new(arg0);
}

GtkPaperSize* Pure_gtk_paper_size_new_from_ppd(char const* arg0, char const* arg1, double arg2, double arg3)
{
  return gtk_paper_size_new_from_ppd(arg0, arg1, arg2, arg3);
}

GtkPaperSize* Pure_gtk_paper_size_new_custom(char const* arg0, char const* arg1, double arg2, double arg3, unsigned int arg4)
{
  return gtk_paper_size_new_custom(arg0, arg1, arg2, arg3, arg4);
}

GtkPaperSize* Pure_gtk_paper_size_copy(GtkPaperSize* arg0)
{
  return gtk_paper_size_copy(arg0);
}

void Pure_gtk_paper_size_free(GtkPaperSize* arg0)
{
  return gtk_paper_size_free(arg0);
}

int Pure_gtk_paper_size_is_equal(GtkPaperSize* arg0, GtkPaperSize* arg1)
{
  return gtk_paper_size_is_equal(arg0, arg1);
}

GList* Pure_gtk_paper_size_get_paper_sizes(int arg0)
{
  return gtk_paper_size_get_paper_sizes(arg0);
}

char const* Pure_gtk_paper_size_get_name(GtkPaperSize* arg0)
{
  return gtk_paper_size_get_name(arg0);
}

char const* Pure_gtk_paper_size_get_display_name(GtkPaperSize* arg0)
{
  return gtk_paper_size_get_display_name(arg0);
}

char const* Pure_gtk_paper_size_get_ppd_name(GtkPaperSize* arg0)
{
  return gtk_paper_size_get_ppd_name(arg0);
}

double Pure_gtk_paper_size_get_width(GtkPaperSize* arg0, unsigned int arg1)
{
  return gtk_paper_size_get_width(arg0, arg1);
}

double Pure_gtk_paper_size_get_height(GtkPaperSize* arg0, unsigned int arg1)
{
  return gtk_paper_size_get_height(arg0, arg1);
}

int Pure_gtk_paper_size_is_custom(GtkPaperSize* arg0)
{
  return gtk_paper_size_is_custom(arg0);
}

void Pure_gtk_paper_size_set_size(GtkPaperSize* arg0, double arg1, double arg2, unsigned int arg3)
{
  return gtk_paper_size_set_size(arg0, arg1, arg2, arg3);
}

double Pure_gtk_paper_size_get_default_top_margin(GtkPaperSize* arg0, unsigned int arg1)
{
  return gtk_paper_size_get_default_top_margin(arg0, arg1);
}

double Pure_gtk_paper_size_get_default_bottom_margin(GtkPaperSize* arg0, unsigned int arg1)
{
  return gtk_paper_size_get_default_bottom_margin(arg0, arg1);
}

double Pure_gtk_paper_size_get_default_left_margin(GtkPaperSize* arg0, unsigned int arg1)
{
  return gtk_paper_size_get_default_left_margin(arg0, arg1);
}

double Pure_gtk_paper_size_get_default_right_margin(GtkPaperSize* arg0, unsigned int arg1)
{
  return gtk_paper_size_get_default_right_margin(arg0, arg1);
}

char const* Pure_gtk_paper_size_get_default()
{
  return gtk_paper_size_get_default();
}

GtkPaperSize* Pure_gtk_paper_size_new_from_key_file(GKeyFile* arg0, char const* arg1, GError** arg2)
{
  return gtk_paper_size_new_from_key_file(arg0, arg1, arg2);
}

void Pure_gtk_paper_size_to_key_file(GtkPaperSize* arg0, GKeyFile* arg1, char const* arg2)
{
  return gtk_paper_size_to_key_file(arg0, arg1, arg2);
}

unsigned long Pure_gtk_page_setup_get_type()
{
  return gtk_page_setup_get_type();
}

GtkPageSetup* Pure_gtk_page_setup_new()
{
  return gtk_page_setup_new();
}

GtkPageSetup* Pure_gtk_page_setup_copy(GtkPageSetup* arg0)
{
  return gtk_page_setup_copy(arg0);
}

unsigned int Pure_gtk_page_setup_get_orientation(GtkPageSetup* arg0)
{
  return gtk_page_setup_get_orientation(arg0);
}

void Pure_gtk_page_setup_set_orientation(GtkPageSetup* arg0, unsigned int arg1)
{
  return gtk_page_setup_set_orientation(arg0, arg1);
}

GtkPaperSize* Pure_gtk_page_setup_get_paper_size(GtkPageSetup* arg0)
{
  return gtk_page_setup_get_paper_size(arg0);
}

void Pure_gtk_page_setup_set_paper_size(GtkPageSetup* arg0, GtkPaperSize* arg1)
{
  return gtk_page_setup_set_paper_size(arg0, arg1);
}

double Pure_gtk_page_setup_get_top_margin(GtkPageSetup* arg0, unsigned int arg1)
{
  return gtk_page_setup_get_top_margin(arg0, arg1);
}

void Pure_gtk_page_setup_set_top_margin(GtkPageSetup* arg0, double arg1, unsigned int arg2)
{
  return gtk_page_setup_set_top_margin(arg0, arg1, arg2);
}

double Pure_gtk_page_setup_get_bottom_margin(GtkPageSetup* arg0, unsigned int arg1)
{
  return gtk_page_setup_get_bottom_margin(arg0, arg1);
}

void Pure_gtk_page_setup_set_bottom_margin(GtkPageSetup* arg0, double arg1, unsigned int arg2)
{
  return gtk_page_setup_set_bottom_margin(arg0, arg1, arg2);
}

double Pure_gtk_page_setup_get_left_margin(GtkPageSetup* arg0, unsigned int arg1)
{
  return gtk_page_setup_get_left_margin(arg0, arg1);
}

void Pure_gtk_page_setup_set_left_margin(GtkPageSetup* arg0, double arg1, unsigned int arg2)
{
  return gtk_page_setup_set_left_margin(arg0, arg1, arg2);
}

double Pure_gtk_page_setup_get_right_margin(GtkPageSetup* arg0, unsigned int arg1)
{
  return gtk_page_setup_get_right_margin(arg0, arg1);
}

void Pure_gtk_page_setup_set_right_margin(GtkPageSetup* arg0, double arg1, unsigned int arg2)
{
  return gtk_page_setup_set_right_margin(arg0, arg1, arg2);
}

void Pure_gtk_page_setup_set_paper_size_and_default_margins(GtkPageSetup* arg0, GtkPaperSize* arg1)
{
  return gtk_page_setup_set_paper_size_and_default_margins(arg0, arg1);
}

double Pure_gtk_page_setup_get_paper_width(GtkPageSetup* arg0, unsigned int arg1)
{
  return gtk_page_setup_get_paper_width(arg0, arg1);
}

double Pure_gtk_page_setup_get_paper_height(GtkPageSetup* arg0, unsigned int arg1)
{
  return gtk_page_setup_get_paper_height(arg0, arg1);
}

double Pure_gtk_page_setup_get_page_width(GtkPageSetup* arg0, unsigned int arg1)
{
  return gtk_page_setup_get_page_width(arg0, arg1);
}

double Pure_gtk_page_setup_get_page_height(GtkPageSetup* arg0, unsigned int arg1)
{
  return gtk_page_setup_get_page_height(arg0, arg1);
}

GtkPageSetup* Pure_gtk_page_setup_new_from_file(char const* arg0, GError** arg1)
{
  return gtk_page_setup_new_from_file(arg0, arg1);
}

int Pure_gtk_page_setup_load_file(GtkPageSetup* arg0, char const* arg1, GError** arg2)
{
  return gtk_page_setup_load_file(arg0, arg1, arg2);
}

int Pure_gtk_page_setup_to_file(GtkPageSetup* arg0, char const* arg1, GError** arg2)
{
  return gtk_page_setup_to_file(arg0, arg1, arg2);
}

GtkPageSetup* Pure_gtk_page_setup_new_from_key_file(GKeyFile* arg0, char const* arg1, GError** arg2)
{
  return gtk_page_setup_new_from_key_file(arg0, arg1, arg2);
}

int Pure_gtk_page_setup_load_key_file(GtkPageSetup* arg0, GKeyFile* arg1, char const* arg2, GError** arg3)
{
  return gtk_page_setup_load_key_file(arg0, arg1, arg2, arg3);
}

void Pure_gtk_page_setup_to_key_file(GtkPageSetup* arg0, GKeyFile* arg1, char const* arg2)
{
  return gtk_page_setup_to_key_file(arg0, arg1, arg2);
}

unsigned long Pure_gtk_socket_get_type()
{
  return gtk_socket_get_type();
}

GtkWidget* Pure_gtk_socket_new()
{
  return gtk_socket_new();
}

void Pure_gtk_socket_add_id(GtkSocket* arg0, unsigned int arg1)
{
  return gtk_socket_add_id(arg0, arg1);
}

unsigned int Pure_gtk_socket_get_id(GtkSocket* arg0)
{
  return gtk_socket_get_id(arg0);
}

GdkWindow* Pure_gtk_socket_get_plug_window(GtkSocket* arg0)
{
  return gtk_socket_get_plug_window(arg0);
}

void Pure_gtk_socket_steal(GtkSocket* arg0, unsigned int arg1)
{
  return gtk_socket_steal(arg0, arg1);
}

unsigned long Pure_gtk_plug_get_type()
{
  return gtk_plug_get_type();
}

void Pure_gtk_plug_construct(GtkPlug* arg0, unsigned int arg1)
{
  return gtk_plug_construct(arg0, arg1);
}

GtkWidget* Pure_gtk_plug_new(unsigned int arg0)
{
  return gtk_plug_new(arg0);
}

void Pure_gtk_plug_construct_for_display(GtkPlug* arg0, GdkDisplay* arg1, unsigned int arg2)
{
  return gtk_plug_construct_for_display(arg0, arg1, arg2);
}

GtkWidget* Pure_gtk_plug_new_for_display(GdkDisplay* arg0, unsigned int arg1)
{
  return gtk_plug_new_for_display(arg0, arg1);
}

unsigned int Pure_gtk_plug_get_id(GtkPlug* arg0)
{
  return gtk_plug_get_id(arg0);
}

int Pure_gtk_plug_get_embedded(GtkPlug* arg0)
{
  return gtk_plug_get_embedded(arg0);
}

GdkWindow* Pure_gtk_plug_get_socket_window(GtkPlug* arg0)
{
  return gtk_plug_get_socket_window(arg0);
}

unsigned long Pure_gtk_print_context_get_type()
{
  return gtk_print_context_get_type();
}

cairo_t* Pure_gtk_print_context_get_cairo_context(GtkPrintContext* arg0)
{
  return gtk_print_context_get_cairo_context(arg0);
}

GtkPageSetup* Pure_gtk_print_context_get_page_setup(GtkPrintContext* arg0)
{
  return gtk_print_context_get_page_setup(arg0);
}

double Pure_gtk_print_context_get_width(GtkPrintContext* arg0)
{
  return gtk_print_context_get_width(arg0);
}

double Pure_gtk_print_context_get_height(GtkPrintContext* arg0)
{
  return gtk_print_context_get_height(arg0);
}

double Pure_gtk_print_context_get_dpi_x(GtkPrintContext* arg0)
{
  return gtk_print_context_get_dpi_x(arg0);
}

double Pure_gtk_print_context_get_dpi_y(GtkPrintContext* arg0)
{
  return gtk_print_context_get_dpi_y(arg0);
}

PangoFontMap* Pure_gtk_print_context_get_pango_fontmap(GtkPrintContext* arg0)
{
  return gtk_print_context_get_pango_fontmap(arg0);
}

PangoContext* Pure_gtk_print_context_create_pango_context(GtkPrintContext* arg0)
{
  return gtk_print_context_create_pango_context(arg0);
}

PangoLayout* Pure_gtk_print_context_create_pango_layout(GtkPrintContext* arg0)
{
  return gtk_print_context_create_pango_layout(arg0);
}

void Pure_gtk_print_context_set_cairo_context(GtkPrintContext* arg0, cairo_t* arg1, double arg2, double arg3)
{
  return gtk_print_context_set_cairo_context(arg0, arg1, arg2, arg3);
}

unsigned long Pure_gtk_print_settings_get_type()
{
  return gtk_print_settings_get_type();
}

GtkPrintSettings* Pure_gtk_print_settings_new()
{
  return gtk_print_settings_new();
}

GtkPrintSettings* Pure_gtk_print_settings_copy(GtkPrintSettings* arg0)
{
  return gtk_print_settings_copy(arg0);
}

GtkPrintSettings* Pure_gtk_print_settings_new_from_file(char const* arg0, GError** arg1)
{
  return gtk_print_settings_new_from_file(arg0, arg1);
}

int Pure_gtk_print_settings_load_file(GtkPrintSettings* arg0, char const* arg1, GError** arg2)
{
  return gtk_print_settings_load_file(arg0, arg1, arg2);
}

int Pure_gtk_print_settings_to_file(GtkPrintSettings* arg0, char const* arg1, GError** arg2)
{
  return gtk_print_settings_to_file(arg0, arg1, arg2);
}

GtkPrintSettings* Pure_gtk_print_settings_new_from_key_file(GKeyFile* arg0, char const* arg1, GError** arg2)
{
  return gtk_print_settings_new_from_key_file(arg0, arg1, arg2);
}

int Pure_gtk_print_settings_load_key_file(GtkPrintSettings* arg0, GKeyFile* arg1, char const* arg2, GError** arg3)
{
  return gtk_print_settings_load_key_file(arg0, arg1, arg2, arg3);
}

void Pure_gtk_print_settings_to_key_file(GtkPrintSettings* arg0, GKeyFile* arg1, char const* arg2)
{
  return gtk_print_settings_to_key_file(arg0, arg1, arg2);
}

int Pure_gtk_print_settings_has_key(GtkPrintSettings* arg0, char const* arg1)
{
  return gtk_print_settings_has_key(arg0, arg1);
}

char const* Pure_gtk_print_settings_get(GtkPrintSettings* arg0, char const* arg1)
{
  return gtk_print_settings_get(arg0, arg1);
}

void Pure_gtk_print_settings_set(GtkPrintSettings* arg0, char const* arg1, char const* arg2)
{
  return gtk_print_settings_set(arg0, arg1, arg2);
}

void Pure_gtk_print_settings_unset(GtkPrintSettings* arg0, char const* arg1)
{
  return gtk_print_settings_unset(arg0, arg1);
}

void Pure_gtk_print_settings_foreach(GtkPrintSettings* arg0, void* arg1, void* arg2)
{
  return gtk_print_settings_foreach(arg0, arg1, arg2);
}

int Pure_gtk_print_settings_get_bool(GtkPrintSettings* arg0, char const* arg1)
{
  return gtk_print_settings_get_bool(arg0, arg1);
}

void Pure_gtk_print_settings_set_bool(GtkPrintSettings* arg0, char const* arg1, int arg2)
{
  return gtk_print_settings_set_bool(arg0, arg1, arg2);
}

double Pure_gtk_print_settings_get_double(GtkPrintSettings* arg0, char const* arg1)
{
  return gtk_print_settings_get_double(arg0, arg1);
}

double Pure_gtk_print_settings_get_double_with_default(GtkPrintSettings* arg0, char const* arg1, double arg2)
{
  return gtk_print_settings_get_double_with_default(arg0, arg1, arg2);
}

void Pure_gtk_print_settings_set_double(GtkPrintSettings* arg0, char const* arg1, double arg2)
{
  return gtk_print_settings_set_double(arg0, arg1, arg2);
}

double Pure_gtk_print_settings_get_length(GtkPrintSettings* arg0, char const* arg1, unsigned int arg2)
{
  return gtk_print_settings_get_length(arg0, arg1, arg2);
}

void Pure_gtk_print_settings_set_length(GtkPrintSettings* arg0, char const* arg1, double arg2, unsigned int arg3)
{
  return gtk_print_settings_set_length(arg0, arg1, arg2, arg3);
}

int Pure_gtk_print_settings_get_int(GtkPrintSettings* arg0, char const* arg1)
{
  return gtk_print_settings_get_int(arg0, arg1);
}

int Pure_gtk_print_settings_get_int_with_default(GtkPrintSettings* arg0, char const* arg1, int arg2)
{
  return gtk_print_settings_get_int_with_default(arg0, arg1, arg2);
}

void Pure_gtk_print_settings_set_int(GtkPrintSettings* arg0, char const* arg1, int arg2)
{
  return gtk_print_settings_set_int(arg0, arg1, arg2);
}

char const* Pure_gtk_print_settings_get_printer(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_printer(arg0);
}

void Pure_gtk_print_settings_set_printer(GtkPrintSettings* arg0, char const* arg1)
{
  return gtk_print_settings_set_printer(arg0, arg1);
}

unsigned int Pure_gtk_print_settings_get_orientation(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_orientation(arg0);
}

void Pure_gtk_print_settings_set_orientation(GtkPrintSettings* arg0, unsigned int arg1)
{
  return gtk_print_settings_set_orientation(arg0, arg1);
}

GtkPaperSize* Pure_gtk_print_settings_get_paper_size(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_paper_size(arg0);
}

void Pure_gtk_print_settings_set_paper_size(GtkPrintSettings* arg0, GtkPaperSize* arg1)
{
  return gtk_print_settings_set_paper_size(arg0, arg1);
}

double Pure_gtk_print_settings_get_paper_width(GtkPrintSettings* arg0, unsigned int arg1)
{
  return gtk_print_settings_get_paper_width(arg0, arg1);
}

void Pure_gtk_print_settings_set_paper_width(GtkPrintSettings* arg0, double arg1, unsigned int arg2)
{
  return gtk_print_settings_set_paper_width(arg0, arg1, arg2);
}

double Pure_gtk_print_settings_get_paper_height(GtkPrintSettings* arg0, unsigned int arg1)
{
  return gtk_print_settings_get_paper_height(arg0, arg1);
}

void Pure_gtk_print_settings_set_paper_height(GtkPrintSettings* arg0, double arg1, unsigned int arg2)
{
  return gtk_print_settings_set_paper_height(arg0, arg1, arg2);
}

int Pure_gtk_print_settings_get_use_color(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_use_color(arg0);
}

void Pure_gtk_print_settings_set_use_color(GtkPrintSettings* arg0, int arg1)
{
  return gtk_print_settings_set_use_color(arg0, arg1);
}

int Pure_gtk_print_settings_get_collate(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_collate(arg0);
}

void Pure_gtk_print_settings_set_collate(GtkPrintSettings* arg0, int arg1)
{
  return gtk_print_settings_set_collate(arg0, arg1);
}

int Pure_gtk_print_settings_get_reverse(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_reverse(arg0);
}

void Pure_gtk_print_settings_set_reverse(GtkPrintSettings* arg0, int arg1)
{
  return gtk_print_settings_set_reverse(arg0, arg1);
}

unsigned int Pure_gtk_print_settings_get_duplex(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_duplex(arg0);
}

void Pure_gtk_print_settings_set_duplex(GtkPrintSettings* arg0, unsigned int arg1)
{
  return gtk_print_settings_set_duplex(arg0, arg1);
}

unsigned int Pure_gtk_print_settings_get_quality(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_quality(arg0);
}

void Pure_gtk_print_settings_set_quality(GtkPrintSettings* arg0, unsigned int arg1)
{
  return gtk_print_settings_set_quality(arg0, arg1);
}

int Pure_gtk_print_settings_get_n_copies(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_n_copies(arg0);
}

void Pure_gtk_print_settings_set_n_copies(GtkPrintSettings* arg0, int arg1)
{
  return gtk_print_settings_set_n_copies(arg0, arg1);
}

int Pure_gtk_print_settings_get_number_up(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_number_up(arg0);
}

void Pure_gtk_print_settings_set_number_up(GtkPrintSettings* arg0, int arg1)
{
  return gtk_print_settings_set_number_up(arg0, arg1);
}

unsigned int Pure_gtk_print_settings_get_number_up_layout(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_number_up_layout(arg0);
}

void Pure_gtk_print_settings_set_number_up_layout(GtkPrintSettings* arg0, unsigned int arg1)
{
  return gtk_print_settings_set_number_up_layout(arg0, arg1);
}

int Pure_gtk_print_settings_get_resolution(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_resolution(arg0);
}

void Pure_gtk_print_settings_set_resolution(GtkPrintSettings* arg0, int arg1)
{
  return gtk_print_settings_set_resolution(arg0, arg1);
}

double Pure_gtk_print_settings_get_scale(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_scale(arg0);
}

void Pure_gtk_print_settings_set_scale(GtkPrintSettings* arg0, double arg1)
{
  return gtk_print_settings_set_scale(arg0, arg1);
}

unsigned int Pure_gtk_print_settings_get_print_pages(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_print_pages(arg0);
}

void Pure_gtk_print_settings_set_print_pages(GtkPrintSettings* arg0, unsigned int arg1)
{
  return gtk_print_settings_set_print_pages(arg0, arg1);
}

GtkPageRange* Pure_gtk_print_settings_get_page_ranges(GtkPrintSettings* arg0, int* arg1)
{
  return gtk_print_settings_get_page_ranges(arg0, arg1);
}

void Pure_gtk_print_settings_set_page_ranges(GtkPrintSettings* arg0, GtkPageRange* arg1, int arg2)
{
  return gtk_print_settings_set_page_ranges(arg0, arg1, arg2);
}

unsigned int Pure_gtk_print_settings_get_page_set(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_page_set(arg0);
}

void Pure_gtk_print_settings_set_page_set(GtkPrintSettings* arg0, unsigned int arg1)
{
  return gtk_print_settings_set_page_set(arg0, arg1);
}

char const* Pure_gtk_print_settings_get_default_source(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_default_source(arg0);
}

void Pure_gtk_print_settings_set_default_source(GtkPrintSettings* arg0, char const* arg1)
{
  return gtk_print_settings_set_default_source(arg0, arg1);
}

char const* Pure_gtk_print_settings_get_media_type(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_media_type(arg0);
}

void Pure_gtk_print_settings_set_media_type(GtkPrintSettings* arg0, char const* arg1)
{
  return gtk_print_settings_set_media_type(arg0, arg1);
}

char const* Pure_gtk_print_settings_get_dither(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_dither(arg0);
}

void Pure_gtk_print_settings_set_dither(GtkPrintSettings* arg0, char const* arg1)
{
  return gtk_print_settings_set_dither(arg0, arg1);
}

char const* Pure_gtk_print_settings_get_finishings(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_finishings(arg0);
}

void Pure_gtk_print_settings_set_finishings(GtkPrintSettings* arg0, char const* arg1)
{
  return gtk_print_settings_set_finishings(arg0, arg1);
}

char const* Pure_gtk_print_settings_get_output_bin(GtkPrintSettings* arg0)
{
  return gtk_print_settings_get_output_bin(arg0);
}

void Pure_gtk_print_settings_set_output_bin(GtkPrintSettings* arg0, char const* arg1)
{
  return gtk_print_settings_set_output_bin(arg0, arg1);
}

unsigned long Pure_gtk_print_operation_preview_get_type()
{
  return gtk_print_operation_preview_get_type();
}

void Pure_gtk_print_operation_preview_render_page(GtkPrintOperationPreview* arg0, int arg1)
{
  return gtk_print_operation_preview_render_page(arg0, arg1);
}

void Pure_gtk_print_operation_preview_end_preview(GtkPrintOperationPreview* arg0)
{
  return gtk_print_operation_preview_end_preview(arg0);
}

int Pure_gtk_print_operation_preview_is_selected(GtkPrintOperationPreview* arg0, int arg1)
{
  return gtk_print_operation_preview_is_selected(arg0, arg1);
}

unsigned int Pure_gtk_print_error_quark()
{
  return gtk_print_error_quark();
}

unsigned long Pure_gtk_print_operation_get_type()
{
  return gtk_print_operation_get_type();
}

GtkPrintOperation* Pure_gtk_print_operation_new()
{
  return gtk_print_operation_new();
}

void Pure_gtk_print_operation_set_default_page_setup(GtkPrintOperation* arg0, GtkPageSetup* arg1)
{
  return gtk_print_operation_set_default_page_setup(arg0, arg1);
}

GtkPageSetup* Pure_gtk_print_operation_get_default_page_setup(GtkPrintOperation* arg0)
{
  return gtk_print_operation_get_default_page_setup(arg0);
}

void Pure_gtk_print_operation_set_print_settings(GtkPrintOperation* arg0, GtkPrintSettings* arg1)
{
  return gtk_print_operation_set_print_settings(arg0, arg1);
}

GtkPrintSettings* Pure_gtk_print_operation_get_print_settings(GtkPrintOperation* arg0)
{
  return gtk_print_operation_get_print_settings(arg0);
}

void Pure_gtk_print_operation_set_job_name(GtkPrintOperation* arg0, char const* arg1)
{
  return gtk_print_operation_set_job_name(arg0, arg1);
}

void Pure_gtk_print_operation_set_n_pages(GtkPrintOperation* arg0, int arg1)
{
  return gtk_print_operation_set_n_pages(arg0, arg1);
}

void Pure_gtk_print_operation_set_current_page(GtkPrintOperation* arg0, int arg1)
{
  return gtk_print_operation_set_current_page(arg0, arg1);
}

void Pure_gtk_print_operation_set_use_full_page(GtkPrintOperation* arg0, int arg1)
{
  return gtk_print_operation_set_use_full_page(arg0, arg1);
}

void Pure_gtk_print_operation_set_unit(GtkPrintOperation* arg0, unsigned int arg1)
{
  return gtk_print_operation_set_unit(arg0, arg1);
}

void Pure_gtk_print_operation_set_export_filename(GtkPrintOperation* arg0, char const* arg1)
{
  return gtk_print_operation_set_export_filename(arg0, arg1);
}

void Pure_gtk_print_operation_set_track_print_status(GtkPrintOperation* arg0, int arg1)
{
  return gtk_print_operation_set_track_print_status(arg0, arg1);
}

void Pure_gtk_print_operation_set_show_progress(GtkPrintOperation* arg0, int arg1)
{
  return gtk_print_operation_set_show_progress(arg0, arg1);
}

void Pure_gtk_print_operation_set_allow_async(GtkPrintOperation* arg0, int arg1)
{
  return gtk_print_operation_set_allow_async(arg0, arg1);
}

void Pure_gtk_print_operation_set_custom_tab_label(GtkPrintOperation* arg0, char const* arg1)
{
  return gtk_print_operation_set_custom_tab_label(arg0, arg1);
}

unsigned int Pure_gtk_print_operation_run(GtkPrintOperation* arg0, unsigned int arg1, GtkWindow* arg2, GError** arg3)
{
  return gtk_print_operation_run(arg0, arg1, arg2, arg3);
}

void Pure_gtk_print_operation_get_error(GtkPrintOperation* arg0, GError** arg1)
{
  return gtk_print_operation_get_error(arg0, arg1);
}

unsigned int Pure_gtk_print_operation_get_status(GtkPrintOperation* arg0)
{
  return gtk_print_operation_get_status(arg0);
}

char const* Pure_gtk_print_operation_get_status_string(GtkPrintOperation* arg0)
{
  return gtk_print_operation_get_status_string(arg0);
}

int Pure_gtk_print_operation_is_finished(GtkPrintOperation* arg0)
{
  return gtk_print_operation_is_finished(arg0);
}

void Pure_gtk_print_operation_cancel(GtkPrintOperation* arg0)
{
  return gtk_print_operation_cancel(arg0);
}

GtkPageSetup* Pure_gtk_print_run_page_setup_dialog(GtkWindow* arg0, GtkPageSetup* arg1, GtkPrintSettings* arg2)
{
  return gtk_print_run_page_setup_dialog(arg0, arg1, arg2);
}

void Pure_gtk_print_run_page_setup_dialog_async(GtkWindow* arg0, GtkPageSetup* arg1, GtkPrintSettings* arg2, void* arg3, void* arg4)
{
  return gtk_print_run_page_setup_dialog_async(arg0, arg1, arg2, arg3, arg4);
}

unsigned long Pure_gtk_progress_get_type()
{
  return gtk_progress_get_type();
}

void Pure_gtk_progress_set_show_text(GtkProgress* arg0, int arg1)
{
  return gtk_progress_set_show_text(arg0, arg1);
}

void Pure_gtk_progress_set_text_alignment(GtkProgress* arg0, float arg1, float arg2)
{
  return gtk_progress_set_text_alignment(arg0, arg1, arg2);
}

void Pure_gtk_progress_set_format_string(GtkProgress* arg0, char const* arg1)
{
  return gtk_progress_set_format_string(arg0, arg1);
}

void Pure_gtk_progress_set_adjustment(GtkProgress* arg0, GtkAdjustment* arg1)
{
  return gtk_progress_set_adjustment(arg0, arg1);
}

void Pure_gtk_progress_configure(GtkProgress* arg0, double arg1, double arg2, double arg3)
{
  return gtk_progress_configure(arg0, arg1, arg2, arg3);
}

void Pure_gtk_progress_set_percentage(GtkProgress* arg0, double arg1)
{
  return gtk_progress_set_percentage(arg0, arg1);
}

void Pure_gtk_progress_set_value(GtkProgress* arg0, double arg1)
{
  return gtk_progress_set_value(arg0, arg1);
}

double Pure_gtk_progress_get_value(GtkProgress* arg0)
{
  return gtk_progress_get_value(arg0);
}

void Pure_gtk_progress_set_activity_mode(GtkProgress* arg0, int arg1)
{
  return gtk_progress_set_activity_mode(arg0, arg1);
}

char* Pure_gtk_progress_get_current_text(GtkProgress* arg0)
{
  return gtk_progress_get_current_text(arg0);
}

char* Pure_gtk_progress_get_text_from_value(GtkProgress* arg0, double arg1)
{
  return gtk_progress_get_text_from_value(arg0, arg1);
}

double Pure_gtk_progress_get_current_percentage(GtkProgress* arg0)
{
  return gtk_progress_get_current_percentage(arg0);
}

double Pure_gtk_progress_get_percentage_from_value(GtkProgress* arg0, double arg1)
{
  return gtk_progress_get_percentage_from_value(arg0, arg1);
}

unsigned long Pure_gtk_progress_bar_get_type()
{
  return gtk_progress_bar_get_type();
}

GtkWidget* Pure_gtk_progress_bar_new()
{
  return gtk_progress_bar_new();
}

void Pure_gtk_progress_bar_pulse(GtkProgressBar* arg0)
{
  return gtk_progress_bar_pulse(arg0);
}

void Pure_gtk_progress_bar_set_text(GtkProgressBar* arg0, char const* arg1)
{
  return gtk_progress_bar_set_text(arg0, arg1);
}

void Pure_gtk_progress_bar_set_fraction(GtkProgressBar* arg0, double arg1)
{
  return gtk_progress_bar_set_fraction(arg0, arg1);
}

void Pure_gtk_progress_bar_set_pulse_step(GtkProgressBar* arg0, double arg1)
{
  return gtk_progress_bar_set_pulse_step(arg0, arg1);
}

void Pure_gtk_progress_bar_set_orientation(GtkProgressBar* arg0, unsigned int arg1)
{
  return gtk_progress_bar_set_orientation(arg0, arg1);
}

char const* Pure_gtk_progress_bar_get_text(GtkProgressBar* arg0)
{
  return gtk_progress_bar_get_text(arg0);
}

double Pure_gtk_progress_bar_get_fraction(GtkProgressBar* arg0)
{
  return gtk_progress_bar_get_fraction(arg0);
}

double Pure_gtk_progress_bar_get_pulse_step(GtkProgressBar* arg0)
{
  return gtk_progress_bar_get_pulse_step(arg0);
}

unsigned int Pure_gtk_progress_bar_get_orientation(GtkProgressBar* arg0)
{
  return gtk_progress_bar_get_orientation(arg0);
}

void Pure_gtk_progress_bar_set_ellipsize(GtkProgressBar* arg0, unsigned int arg1)
{
  return gtk_progress_bar_set_ellipsize(arg0, arg1);
}

unsigned int Pure_gtk_progress_bar_get_ellipsize(GtkProgressBar* arg0)
{
  return gtk_progress_bar_get_ellipsize(arg0);
}

GtkWidget* Pure_gtk_progress_bar_new_with_adjustment(GtkAdjustment* arg0)
{
  return gtk_progress_bar_new_with_adjustment(arg0);
}

void Pure_gtk_progress_bar_set_bar_style(GtkProgressBar* arg0, unsigned int arg1)
{
  return gtk_progress_bar_set_bar_style(arg0, arg1);
}

void Pure_gtk_progress_bar_set_discrete_blocks(GtkProgressBar* arg0, unsigned int arg1)
{
  return gtk_progress_bar_set_discrete_blocks(arg0, arg1);
}

void Pure_gtk_progress_bar_set_activity_step(GtkProgressBar* arg0, unsigned int arg1)
{
  return gtk_progress_bar_set_activity_step(arg0, arg1);
}

void Pure_gtk_progress_bar_set_activity_blocks(GtkProgressBar* arg0, unsigned int arg1)
{
  return gtk_progress_bar_set_activity_blocks(arg0, arg1);
}

void Pure_gtk_progress_bar_update(GtkProgressBar* arg0, double arg1)
{
  return gtk_progress_bar_update(arg0, arg1);
}

unsigned long Pure_gtk_toggle_action_get_type()
{
  return gtk_toggle_action_get_type();
}

GtkToggleAction* Pure_gtk_toggle_action_new(char const* arg0, char const* arg1, char const* arg2, char const* arg3)
{
  return gtk_toggle_action_new(arg0, arg1, arg2, arg3);
}

void Pure_gtk_toggle_action_toggled(GtkToggleAction* arg0)
{
  return gtk_toggle_action_toggled(arg0);
}

void Pure_gtk_toggle_action_set_active(GtkToggleAction* arg0, int arg1)
{
  return gtk_toggle_action_set_active(arg0, arg1);
}

int Pure_gtk_toggle_action_get_active(GtkToggleAction* arg0)
{
  return gtk_toggle_action_get_active(arg0);
}

void Pure_gtk_toggle_action_set_draw_as_radio(GtkToggleAction* arg0, int arg1)
{
  return gtk_toggle_action_set_draw_as_radio(arg0, arg1);
}

int Pure_gtk_toggle_action_get_draw_as_radio(GtkToggleAction* arg0)
{
  return gtk_toggle_action_get_draw_as_radio(arg0);
}

unsigned long Pure_gtk_radio_action_get_type()
{
  return gtk_radio_action_get_type();
}

GtkRadioAction* Pure_gtk_radio_action_new(char const* arg0, char const* arg1, char const* arg2, char const* arg3, int arg4)
{
  return gtk_radio_action_new(arg0, arg1, arg2, arg3, arg4);
}

GSList* Pure_gtk_radio_action_get_group(GtkRadioAction* arg0)
{
  return gtk_radio_action_get_group(arg0);
}

void Pure_gtk_radio_action_set_group(GtkRadioAction* arg0, GSList* arg1)
{
  return gtk_radio_action_set_group(arg0, arg1);
}

int Pure_gtk_radio_action_get_current_value(GtkRadioAction* arg0)
{
  return gtk_radio_action_get_current_value(arg0);
}

void Pure_gtk_radio_action_set_current_value(GtkRadioAction* arg0, int arg1)
{
  return gtk_radio_action_set_current_value(arg0, arg1);
}

unsigned long Pure_gtk_radio_button_get_type()
{
  return gtk_radio_button_get_type();
}

GtkWidget* Pure_gtk_radio_button_new(GSList* arg0)
{
  return gtk_radio_button_new(arg0);
}

GtkWidget* Pure_gtk_radio_button_new_from_widget(GtkRadioButton* arg0)
{
  return gtk_radio_button_new_from_widget(arg0);
}

GtkWidget* Pure_gtk_radio_button_new_with_label(GSList* arg0, char const* arg1)
{
  return gtk_radio_button_new_with_label(arg0, arg1);
}

GtkWidget* Pure_gtk_radio_button_new_with_label_from_widget(GtkRadioButton* arg0, char const* arg1)
{
  return gtk_radio_button_new_with_label_from_widget(arg0, arg1);
}

GtkWidget* Pure_gtk_radio_button_new_with_mnemonic(GSList* arg0, char const* arg1)
{
  return gtk_radio_button_new_with_mnemonic(arg0, arg1);
}

GtkWidget* Pure_gtk_radio_button_new_with_mnemonic_from_widget(GtkRadioButton* arg0, char const* arg1)
{
  return gtk_radio_button_new_with_mnemonic_from_widget(arg0, arg1);
}

GSList* Pure_gtk_radio_button_get_group(GtkRadioButton* arg0)
{
  return gtk_radio_button_get_group(arg0);
}

void Pure_gtk_radio_button_set_group(GtkRadioButton* arg0, GSList* arg1)
{
  return gtk_radio_button_set_group(arg0, arg1);
}

unsigned long Pure_gtk_radio_menu_item_get_type()
{
  return gtk_radio_menu_item_get_type();
}

GtkWidget* Pure_gtk_radio_menu_item_new(GSList* arg0)
{
  return gtk_radio_menu_item_new(arg0);
}

GtkWidget* Pure_gtk_radio_menu_item_new_with_label(GSList* arg0, char const* arg1)
{
  return gtk_radio_menu_item_new_with_label(arg0, arg1);
}

GtkWidget* Pure_gtk_radio_menu_item_new_with_mnemonic(GSList* arg0, char const* arg1)
{
  return gtk_radio_menu_item_new_with_mnemonic(arg0, arg1);
}

GtkWidget* Pure_gtk_radio_menu_item_new_from_widget(GtkRadioMenuItem* arg0)
{
  return gtk_radio_menu_item_new_from_widget(arg0);
}

GtkWidget* Pure_gtk_radio_menu_item_new_with_mnemonic_from_widget(GtkRadioMenuItem* arg0, char const* arg1)
{
  return gtk_radio_menu_item_new_with_mnemonic_from_widget(arg0, arg1);
}

GtkWidget* Pure_gtk_radio_menu_item_new_with_label_from_widget(GtkRadioMenuItem* arg0, char const* arg1)
{
  return gtk_radio_menu_item_new_with_label_from_widget(arg0, arg1);
}

GSList* Pure_gtk_radio_menu_item_get_group(GtkRadioMenuItem* arg0)
{
  return gtk_radio_menu_item_get_group(arg0);
}

void Pure_gtk_radio_menu_item_set_group(GtkRadioMenuItem* arg0, GSList* arg1)
{
  return gtk_radio_menu_item_set_group(arg0, arg1);
}

unsigned long Pure_gtk_toggle_tool_button_get_type()
{
  return gtk_toggle_tool_button_get_type();
}

GtkToolItem* Pure_gtk_toggle_tool_button_new()
{
  return gtk_toggle_tool_button_new();
}

GtkToolItem* Pure_gtk_toggle_tool_button_new_from_stock(char const* arg0)
{
  return gtk_toggle_tool_button_new_from_stock(arg0);
}

void Pure_gtk_toggle_tool_button_set_active(GtkToggleToolButton* arg0, int arg1)
{
  return gtk_toggle_tool_button_set_active(arg0, arg1);
}

int Pure_gtk_toggle_tool_button_get_active(GtkToggleToolButton* arg0)
{
  return gtk_toggle_tool_button_get_active(arg0);
}

unsigned long Pure_gtk_radio_tool_button_get_type()
{
  return gtk_radio_tool_button_get_type();
}

GtkToolItem* Pure_gtk_radio_tool_button_new(GSList* arg0)
{
  return gtk_radio_tool_button_new(arg0);
}

GtkToolItem* Pure_gtk_radio_tool_button_new_from_stock(GSList* arg0, char const* arg1)
{
  return gtk_radio_tool_button_new_from_stock(arg0, arg1);
}

GtkToolItem* Pure_gtk_radio_tool_button_new_from_widget(GtkRadioToolButton* arg0)
{
  return gtk_radio_tool_button_new_from_widget(arg0);
}

GtkToolItem* Pure_gtk_radio_tool_button_new_with_stock_from_widget(GtkRadioToolButton* arg0, char const* arg1)
{
  return gtk_radio_tool_button_new_with_stock_from_widget(arg0, arg1);
}

GSList* Pure_gtk_radio_tool_button_get_group(GtkRadioToolButton* arg0)
{
  return gtk_radio_tool_button_get_group(arg0);
}

void Pure_gtk_radio_tool_button_set_group(GtkRadioToolButton* arg0, GSList* arg1)
{
  return gtk_radio_tool_button_set_group(arg0, arg1);
}

unsigned int Pure_gtk_recent_manager_error_quark()
{
  return gtk_recent_manager_error_quark();
}

unsigned long Pure_gtk_recent_manager_get_type()
{
  return gtk_recent_manager_get_type();
}

GtkRecentManager* Pure_gtk_recent_manager_new()
{
  return gtk_recent_manager_new();
}

GtkRecentManager* Pure_gtk_recent_manager_get_default()
{
  return gtk_recent_manager_get_default();
}

GtkRecentManager* Pure_gtk_recent_manager_get_for_screen(GdkScreen* arg0)
{
  return gtk_recent_manager_get_for_screen(arg0);
}

void Pure_gtk_recent_manager_set_screen(GtkRecentManager* arg0, GdkScreen* arg1)
{
  return gtk_recent_manager_set_screen(arg0, arg1);
}

int Pure_gtk_recent_manager_add_item(GtkRecentManager* arg0, char const* arg1)
{
  return gtk_recent_manager_add_item(arg0, arg1);
}

int Pure_gtk_recent_manager_add_full(GtkRecentManager* arg0, char const* arg1, GtkRecentData const* arg2)
{
  return gtk_recent_manager_add_full(arg0, arg1, arg2);
}

int Pure_gtk_recent_manager_remove_item(GtkRecentManager* arg0, char const* arg1, GError** arg2)
{
  return gtk_recent_manager_remove_item(arg0, arg1, arg2);
}

GtkRecentInfo* Pure_gtk_recent_manager_lookup_item(GtkRecentManager* arg0, char const* arg1, GError** arg2)
{
  return gtk_recent_manager_lookup_item(arg0, arg1, arg2);
}

int Pure_gtk_recent_manager_has_item(GtkRecentManager* arg0, char const* arg1)
{
  return gtk_recent_manager_has_item(arg0, arg1);
}

int Pure_gtk_recent_manager_move_item(GtkRecentManager* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return gtk_recent_manager_move_item(arg0, arg1, arg2, arg3);
}

void Pure_gtk_recent_manager_set_limit(GtkRecentManager* arg0, int arg1)
{
  return gtk_recent_manager_set_limit(arg0, arg1);
}

int Pure_gtk_recent_manager_get_limit(GtkRecentManager* arg0)
{
  return gtk_recent_manager_get_limit(arg0);
}

GList* Pure_gtk_recent_manager_get_items(GtkRecentManager* arg0)
{
  return gtk_recent_manager_get_items(arg0);
}

int Pure_gtk_recent_manager_purge_items(GtkRecentManager* arg0, GError** arg1)
{
  return gtk_recent_manager_purge_items(arg0, arg1);
}

unsigned long Pure_gtk_recent_info_get_type()
{
  return gtk_recent_info_get_type();
}

GtkRecentInfo* Pure_gtk_recent_info_ref(GtkRecentInfo* arg0)
{
  return gtk_recent_info_ref(arg0);
}

void Pure_gtk_recent_info_unref(GtkRecentInfo* arg0)
{
  return gtk_recent_info_unref(arg0);
}

char const* Pure_gtk_recent_info_get_uri(GtkRecentInfo* arg0)
{
  return gtk_recent_info_get_uri(arg0);
}

char const* Pure_gtk_recent_info_get_display_name(GtkRecentInfo* arg0)
{
  return gtk_recent_info_get_display_name(arg0);
}

char const* Pure_gtk_recent_info_get_description(GtkRecentInfo* arg0)
{
  return gtk_recent_info_get_description(arg0);
}

char const* Pure_gtk_recent_info_get_mime_type(GtkRecentInfo* arg0)
{
  return gtk_recent_info_get_mime_type(arg0);
}

long Pure_gtk_recent_info_get_added(GtkRecentInfo* arg0)
{
  return gtk_recent_info_get_added(arg0);
}

long Pure_gtk_recent_info_get_modified(GtkRecentInfo* arg0)
{
  return gtk_recent_info_get_modified(arg0);
}

long Pure_gtk_recent_info_get_visited(GtkRecentInfo* arg0)
{
  return gtk_recent_info_get_visited(arg0);
}

int Pure_gtk_recent_info_get_private_hint(GtkRecentInfo* arg0)
{
  return gtk_recent_info_get_private_hint(arg0);
}

int Pure_gtk_recent_info_get_application_info(GtkRecentInfo* arg0, char const* arg1, char** arg2, unsigned int* arg3, long* arg4)
{
  return gtk_recent_info_get_application_info(arg0, arg1, arg2, arg3, arg4);
}

char** Pure_gtk_recent_info_get_applications(GtkRecentInfo* arg0, unsigned long* arg1)
{
  return gtk_recent_info_get_applications(arg0, arg1);
}

char* Pure_gtk_recent_info_last_application(GtkRecentInfo* arg0)
{
  return gtk_recent_info_last_application(arg0);
}

int Pure_gtk_recent_info_has_application(GtkRecentInfo* arg0, char const* arg1)
{
  return gtk_recent_info_has_application(arg0, arg1);
}

char** Pure_gtk_recent_info_get_groups(GtkRecentInfo* arg0, unsigned long* arg1)
{
  return gtk_recent_info_get_groups(arg0, arg1);
}

int Pure_gtk_recent_info_has_group(GtkRecentInfo* arg0, char const* arg1)
{
  return gtk_recent_info_has_group(arg0, arg1);
}

GdkPixbuf* Pure_gtk_recent_info_get_icon(GtkRecentInfo* arg0, int arg1)
{
  return gtk_recent_info_get_icon(arg0, arg1);
}

char* Pure_gtk_recent_info_get_short_name(GtkRecentInfo* arg0)
{
  return gtk_recent_info_get_short_name(arg0);
}

char* Pure_gtk_recent_info_get_uri_display(GtkRecentInfo* arg0)
{
  return gtk_recent_info_get_uri_display(arg0);
}

int Pure_gtk_recent_info_get_age(GtkRecentInfo* arg0)
{
  return gtk_recent_info_get_age(arg0);
}

int Pure_gtk_recent_info_is_local(GtkRecentInfo* arg0)
{
  return gtk_recent_info_is_local(arg0);
}

int Pure_gtk_recent_info_exists(GtkRecentInfo* arg0)
{
  return gtk_recent_info_exists(arg0);
}

int Pure_gtk_recent_info_match(GtkRecentInfo* arg0, GtkRecentInfo* arg1)
{
  return gtk_recent_info_match(arg0, arg1);
}

unsigned long Pure_gtk_recent_action_get_type()
{
  return gtk_recent_action_get_type();
}

GtkAction* Pure_gtk_recent_action_new(char const* arg0, char const* arg1, char const* arg2, char const* arg3)
{
  return gtk_recent_action_new(arg0, arg1, arg2, arg3);
}

GtkAction* Pure_gtk_recent_action_new_for_manager(char const* arg0, char const* arg1, char const* arg2, char const* arg3, GtkRecentManager* arg4)
{
  return gtk_recent_action_new_for_manager(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gtk_recent_action_get_show_numbers(GtkRecentAction* arg0)
{
  return gtk_recent_action_get_show_numbers(arg0);
}

void Pure_gtk_recent_action_set_show_numbers(GtkRecentAction* arg0, int arg1)
{
  return gtk_recent_action_set_show_numbers(arg0, arg1);
}

unsigned long Pure_gtk_recent_filter_get_type()
{
  return gtk_recent_filter_get_type();
}

GtkRecentFilter* Pure_gtk_recent_filter_new()
{
  return gtk_recent_filter_new();
}

void Pure_gtk_recent_filter_set_name(GtkRecentFilter* arg0, char const* arg1)
{
  return gtk_recent_filter_set_name(arg0, arg1);
}

char const* Pure_gtk_recent_filter_get_name(GtkRecentFilter* arg0)
{
  return gtk_recent_filter_get_name(arg0);
}

void Pure_gtk_recent_filter_add_mime_type(GtkRecentFilter* arg0, char const* arg1)
{
  return gtk_recent_filter_add_mime_type(arg0, arg1);
}

void Pure_gtk_recent_filter_add_pattern(GtkRecentFilter* arg0, char const* arg1)
{
  return gtk_recent_filter_add_pattern(arg0, arg1);
}

void Pure_gtk_recent_filter_add_pixbuf_formats(GtkRecentFilter* arg0)
{
  return gtk_recent_filter_add_pixbuf_formats(arg0);
}

void Pure_gtk_recent_filter_add_application(GtkRecentFilter* arg0, char const* arg1)
{
  return gtk_recent_filter_add_application(arg0, arg1);
}

void Pure_gtk_recent_filter_add_group(GtkRecentFilter* arg0, char const* arg1)
{
  return gtk_recent_filter_add_group(arg0, arg1);
}

void Pure_gtk_recent_filter_add_age(GtkRecentFilter* arg0, int arg1)
{
  return gtk_recent_filter_add_age(arg0, arg1);
}

void Pure_gtk_recent_filter_add_custom(GtkRecentFilter* arg0, unsigned int arg1, void* arg2, void* arg3, void* arg4)
{
  return gtk_recent_filter_add_custom(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_gtk_recent_filter_get_needed(GtkRecentFilter* arg0)
{
  return gtk_recent_filter_get_needed(arg0);
}

int Pure_gtk_recent_filter_filter(GtkRecentFilter* arg0, GtkRecentFilterInfo const* arg1)
{
  return gtk_recent_filter_filter(arg0, arg1);
}

unsigned int Pure_gtk_recent_chooser_error_quark()
{
  return gtk_recent_chooser_error_quark();
}

unsigned long Pure_gtk_recent_chooser_get_type()
{
  return gtk_recent_chooser_get_type();
}

void Pure_gtk_recent_chooser_set_show_private(GtkRecentChooser* arg0, int arg1)
{
  return gtk_recent_chooser_set_show_private(arg0, arg1);
}

int Pure_gtk_recent_chooser_get_show_private(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_get_show_private(arg0);
}

void Pure_gtk_recent_chooser_set_show_not_found(GtkRecentChooser* arg0, int arg1)
{
  return gtk_recent_chooser_set_show_not_found(arg0, arg1);
}

int Pure_gtk_recent_chooser_get_show_not_found(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_get_show_not_found(arg0);
}

void Pure_gtk_recent_chooser_set_select_multiple(GtkRecentChooser* arg0, int arg1)
{
  return gtk_recent_chooser_set_select_multiple(arg0, arg1);
}

int Pure_gtk_recent_chooser_get_select_multiple(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_get_select_multiple(arg0);
}

void Pure_gtk_recent_chooser_set_limit(GtkRecentChooser* arg0, int arg1)
{
  return gtk_recent_chooser_set_limit(arg0, arg1);
}

int Pure_gtk_recent_chooser_get_limit(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_get_limit(arg0);
}

void Pure_gtk_recent_chooser_set_local_only(GtkRecentChooser* arg0, int arg1)
{
  return gtk_recent_chooser_set_local_only(arg0, arg1);
}

int Pure_gtk_recent_chooser_get_local_only(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_get_local_only(arg0);
}

void Pure_gtk_recent_chooser_set_show_tips(GtkRecentChooser* arg0, int arg1)
{
  return gtk_recent_chooser_set_show_tips(arg0, arg1);
}

int Pure_gtk_recent_chooser_get_show_tips(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_get_show_tips(arg0);
}

void Pure_gtk_recent_chooser_set_show_numbers(GtkRecentChooser* arg0, int arg1)
{
  return gtk_recent_chooser_set_show_numbers(arg0, arg1);
}

int Pure_gtk_recent_chooser_get_show_numbers(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_get_show_numbers(arg0);
}

void Pure_gtk_recent_chooser_set_show_icons(GtkRecentChooser* arg0, int arg1)
{
  return gtk_recent_chooser_set_show_icons(arg0, arg1);
}

int Pure_gtk_recent_chooser_get_show_icons(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_get_show_icons(arg0);
}

void Pure_gtk_recent_chooser_set_sort_type(GtkRecentChooser* arg0, unsigned int arg1)
{
  return gtk_recent_chooser_set_sort_type(arg0, arg1);
}

unsigned int Pure_gtk_recent_chooser_get_sort_type(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_get_sort_type(arg0);
}

void Pure_gtk_recent_chooser_set_sort_func(GtkRecentChooser* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_recent_chooser_set_sort_func(arg0, arg1, arg2, arg3);
}

int Pure_gtk_recent_chooser_set_current_uri(GtkRecentChooser* arg0, char const* arg1, GError** arg2)
{
  return gtk_recent_chooser_set_current_uri(arg0, arg1, arg2);
}

char* Pure_gtk_recent_chooser_get_current_uri(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_get_current_uri(arg0);
}

GtkRecentInfo* Pure_gtk_recent_chooser_get_current_item(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_get_current_item(arg0);
}

int Pure_gtk_recent_chooser_select_uri(GtkRecentChooser* arg0, char const* arg1, GError** arg2)
{
  return gtk_recent_chooser_select_uri(arg0, arg1, arg2);
}

void Pure_gtk_recent_chooser_unselect_uri(GtkRecentChooser* arg0, char const* arg1)
{
  return gtk_recent_chooser_unselect_uri(arg0, arg1);
}

void Pure_gtk_recent_chooser_select_all(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_select_all(arg0);
}

void Pure_gtk_recent_chooser_unselect_all(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_unselect_all(arg0);
}

GList* Pure_gtk_recent_chooser_get_items(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_get_items(arg0);
}

char** Pure_gtk_recent_chooser_get_uris(GtkRecentChooser* arg0, unsigned long* arg1)
{
  return gtk_recent_chooser_get_uris(arg0, arg1);
}

void Pure_gtk_recent_chooser_add_filter(GtkRecentChooser* arg0, GtkRecentFilter* arg1)
{
  return gtk_recent_chooser_add_filter(arg0, arg1);
}

void Pure_gtk_recent_chooser_remove_filter(GtkRecentChooser* arg0, GtkRecentFilter* arg1)
{
  return gtk_recent_chooser_remove_filter(arg0, arg1);
}

GSList* Pure_gtk_recent_chooser_list_filters(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_list_filters(arg0);
}

void Pure_gtk_recent_chooser_set_filter(GtkRecentChooser* arg0, GtkRecentFilter* arg1)
{
  return gtk_recent_chooser_set_filter(arg0, arg1);
}

GtkRecentFilter* Pure_gtk_recent_chooser_get_filter(GtkRecentChooser* arg0)
{
  return gtk_recent_chooser_get_filter(arg0);
}

unsigned long Pure_gtk_recent_chooser_dialog_get_type()
{
  return gtk_recent_chooser_dialog_get_type();
}

GtkWidget* Pure_gtk_recent_chooser_dialog_new(char const* arg0, GtkWindow* arg1, char const* arg2)
{
  return gtk_recent_chooser_dialog_new(arg0, arg1, arg2);
}

GtkWidget* Pure_gtk_recent_chooser_dialog_new_for_manager(char const* arg0, GtkWindow* arg1, GtkRecentManager* arg2, char const* arg3)
{
  return gtk_recent_chooser_dialog_new_for_manager(arg0, arg1, arg2, arg3);
}

unsigned long Pure_gtk_recent_chooser_menu_get_type()
{
  return gtk_recent_chooser_menu_get_type();
}

GtkWidget* Pure_gtk_recent_chooser_menu_new()
{
  return gtk_recent_chooser_menu_new();
}

GtkWidget* Pure_gtk_recent_chooser_menu_new_for_manager(GtkRecentManager* arg0)
{
  return gtk_recent_chooser_menu_new_for_manager(arg0);
}

int Pure_gtk_recent_chooser_menu_get_show_numbers(GtkRecentChooserMenu* arg0)
{
  return gtk_recent_chooser_menu_get_show_numbers(arg0);
}

void Pure_gtk_recent_chooser_menu_set_show_numbers(GtkRecentChooserMenu* arg0, int arg1)
{
  return gtk_recent_chooser_menu_set_show_numbers(arg0, arg1);
}

unsigned long Pure_gtk_recent_chooser_widget_get_type()
{
  return gtk_recent_chooser_widget_get_type();
}

GtkWidget* Pure_gtk_recent_chooser_widget_new()
{
  return gtk_recent_chooser_widget_new();
}

GtkWidget* Pure_gtk_recent_chooser_widget_new_for_manager(GtkRecentManager* arg0)
{
  return gtk_recent_chooser_widget_new_for_manager(arg0);
}

unsigned long Pure_gtk_scale_button_get_type()
{
  return gtk_scale_button_get_type();
}

GtkWidget* Pure_gtk_scale_button_new(unsigned int arg0, double arg1, double arg2, double arg3, char const** arg4)
{
  return gtk_scale_button_new(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_scale_button_set_icons(GtkScaleButton* arg0, char const** arg1)
{
  return gtk_scale_button_set_icons(arg0, arg1);
}

double Pure_gtk_scale_button_get_value(GtkScaleButton* arg0)
{
  return gtk_scale_button_get_value(arg0);
}

void Pure_gtk_scale_button_set_value(GtkScaleButton* arg0, double arg1)
{
  return gtk_scale_button_set_value(arg0, arg1);
}

GtkAdjustment* Pure_gtk_scale_button_get_adjustment(GtkScaleButton* arg0)
{
  return gtk_scale_button_get_adjustment(arg0);
}

void Pure_gtk_scale_button_set_adjustment(GtkScaleButton* arg0, GtkAdjustment* arg1)
{
  return gtk_scale_button_set_adjustment(arg0, arg1);
}

unsigned int Pure_gtk_scale_button_get_orientation(GtkScaleButton* arg0)
{
  return gtk_scale_button_get_orientation(arg0);
}

void Pure_gtk_scale_button_set_orientation(GtkScaleButton* arg0, unsigned int arg1)
{
  return gtk_scale_button_set_orientation(arg0, arg1);
}

GtkWidget* Pure_gtk_scale_button_get_plus_button(GtkScaleButton* arg0)
{
  return gtk_scale_button_get_plus_button(arg0);
}

GtkWidget* Pure_gtk_scale_button_get_minus_button(GtkScaleButton* arg0)
{
  return gtk_scale_button_get_minus_button(arg0);
}

GtkWidget* Pure_gtk_scale_button_get_popup(GtkScaleButton* arg0)
{
  return gtk_scale_button_get_popup(arg0);
}

unsigned long Pure_gtk_vscrollbar_get_type()
{
  return gtk_vscrollbar_get_type();
}

GtkWidget* Pure_gtk_vscrollbar_new(GtkAdjustment* arg0)
{
  return gtk_vscrollbar_new(arg0);
}

unsigned long Pure_gtk_viewport_get_type()
{
  return gtk_viewport_get_type();
}

GtkWidget* Pure_gtk_viewport_new(GtkAdjustment* arg0, GtkAdjustment* arg1)
{
  return gtk_viewport_new(arg0, arg1);
}

GtkAdjustment* Pure_gtk_viewport_get_hadjustment(GtkViewport* arg0)
{
  return gtk_viewport_get_hadjustment(arg0);
}

GtkAdjustment* Pure_gtk_viewport_get_vadjustment(GtkViewport* arg0)
{
  return gtk_viewport_get_vadjustment(arg0);
}

void Pure_gtk_viewport_set_hadjustment(GtkViewport* arg0, GtkAdjustment* arg1)
{
  return gtk_viewport_set_hadjustment(arg0, arg1);
}

void Pure_gtk_viewport_set_vadjustment(GtkViewport* arg0, GtkAdjustment* arg1)
{
  return gtk_viewport_set_vadjustment(arg0, arg1);
}

void Pure_gtk_viewport_set_shadow_type(GtkViewport* arg0, unsigned int arg1)
{
  return gtk_viewport_set_shadow_type(arg0, arg1);
}

unsigned int Pure_gtk_viewport_get_shadow_type(GtkViewport* arg0)
{
  return gtk_viewport_get_shadow_type(arg0);
}

unsigned long Pure_gtk_scrolled_window_get_type()
{
  return gtk_scrolled_window_get_type();
}

GtkWidget* Pure_gtk_scrolled_window_new(GtkAdjustment* arg0, GtkAdjustment* arg1)
{
  return gtk_scrolled_window_new(arg0, arg1);
}

void Pure_gtk_scrolled_window_set_hadjustment(GtkScrolledWindow* arg0, GtkAdjustment* arg1)
{
  return gtk_scrolled_window_set_hadjustment(arg0, arg1);
}

void Pure_gtk_scrolled_window_set_vadjustment(GtkScrolledWindow* arg0, GtkAdjustment* arg1)
{
  return gtk_scrolled_window_set_vadjustment(arg0, arg1);
}

GtkAdjustment* Pure_gtk_scrolled_window_get_hadjustment(GtkScrolledWindow* arg0)
{
  return gtk_scrolled_window_get_hadjustment(arg0);
}

GtkAdjustment* Pure_gtk_scrolled_window_get_vadjustment(GtkScrolledWindow* arg0)
{
  return gtk_scrolled_window_get_vadjustment(arg0);
}

GtkWidget* Pure_gtk_scrolled_window_get_hscrollbar(GtkScrolledWindow* arg0)
{
  return gtk_scrolled_window_get_hscrollbar(arg0);
}

GtkWidget* Pure_gtk_scrolled_window_get_vscrollbar(GtkScrolledWindow* arg0)
{
  return gtk_scrolled_window_get_vscrollbar(arg0);
}

void Pure_gtk_scrolled_window_set_policy(GtkScrolledWindow* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_scrolled_window_set_policy(arg0, arg1, arg2);
}

void Pure_gtk_scrolled_window_get_policy(GtkScrolledWindow* arg0, unsigned int* arg1, unsigned int* arg2)
{
  return gtk_scrolled_window_get_policy(arg0, arg1, arg2);
}

void Pure_gtk_scrolled_window_set_placement(GtkScrolledWindow* arg0, unsigned int arg1)
{
  return gtk_scrolled_window_set_placement(arg0, arg1);
}

void Pure_gtk_scrolled_window_unset_placement(GtkScrolledWindow* arg0)
{
  return gtk_scrolled_window_unset_placement(arg0);
}

unsigned int Pure_gtk_scrolled_window_get_placement(GtkScrolledWindow* arg0)
{
  return gtk_scrolled_window_get_placement(arg0);
}

void Pure_gtk_scrolled_window_set_shadow_type(GtkScrolledWindow* arg0, unsigned int arg1)
{
  return gtk_scrolled_window_set_shadow_type(arg0, arg1);
}

unsigned int Pure_gtk_scrolled_window_get_shadow_type(GtkScrolledWindow* arg0)
{
  return gtk_scrolled_window_get_shadow_type(arg0);
}

void Pure_gtk_scrolled_window_add_with_viewport(GtkScrolledWindow* arg0, GtkWidget* arg1)
{
  return gtk_scrolled_window_add_with_viewport(arg0, arg1);
}

unsigned long Pure_gtk_separator_menu_item_get_type()
{
  return gtk_separator_menu_item_get_type();
}

GtkWidget* Pure_gtk_separator_menu_item_new()
{
  return gtk_separator_menu_item_new();
}

unsigned long Pure_gtk_separator_tool_item_get_type()
{
  return gtk_separator_tool_item_get_type();
}

GtkToolItem* Pure_gtk_separator_tool_item_new()
{
  return gtk_separator_tool_item_new();
}

int Pure_gtk_separator_tool_item_get_draw(GtkSeparatorToolItem* arg0)
{
  return gtk_separator_tool_item_get_draw(arg0);
}

void Pure_gtk_separator_tool_item_set_draw(GtkSeparatorToolItem* arg0, int arg1)
{
  return gtk_separator_tool_item_set_draw(arg0, arg1);
}

int Pure_gtk_show_uri(GdkScreen* arg0, char const* arg1, unsigned int arg2, GError** arg3)
{
  return gtk_show_uri(arg0, arg1, arg2, arg3);
}

unsigned long Pure_gtk_size_group_get_type()
{
  return gtk_size_group_get_type();
}

GtkSizeGroup* Pure_gtk_size_group_new(unsigned int arg0)
{
  return gtk_size_group_new(arg0);
}

void Pure_gtk_size_group_set_mode(GtkSizeGroup* arg0, unsigned int arg1)
{
  return gtk_size_group_set_mode(arg0, arg1);
}

unsigned int Pure_gtk_size_group_get_mode(GtkSizeGroup* arg0)
{
  return gtk_size_group_get_mode(arg0);
}

void Pure_gtk_size_group_set_ignore_hidden(GtkSizeGroup* arg0, int arg1)
{
  return gtk_size_group_set_ignore_hidden(arg0, arg1);
}

int Pure_gtk_size_group_get_ignore_hidden(GtkSizeGroup* arg0)
{
  return gtk_size_group_get_ignore_hidden(arg0);
}

void Pure_gtk_size_group_add_widget(GtkSizeGroup* arg0, GtkWidget* arg1)
{
  return gtk_size_group_add_widget(arg0, arg1);
}

void Pure_gtk_size_group_remove_widget(GtkSizeGroup* arg0, GtkWidget* arg1)
{
  return gtk_size_group_remove_widget(arg0, arg1);
}

GSList* Pure_gtk_size_group_get_widgets(GtkSizeGroup* arg0)
{
  return gtk_size_group_get_widgets(arg0);
}

unsigned long Pure_gtk_spin_button_get_type()
{
  return gtk_spin_button_get_type();
}

void Pure_gtk_spin_button_configure(GtkSpinButton* arg0, GtkAdjustment* arg1, double arg2, unsigned int arg3)
{
  return gtk_spin_button_configure(arg0, arg1, arg2, arg3);
}

GtkWidget* Pure_gtk_spin_button_new(GtkAdjustment* arg0, double arg1, unsigned int arg2)
{
  return gtk_spin_button_new(arg0, arg1, arg2);
}

GtkWidget* Pure_gtk_spin_button_new_with_range(double arg0, double arg1, double arg2)
{
  return gtk_spin_button_new_with_range(arg0, arg1, arg2);
}

void Pure_gtk_spin_button_set_adjustment(GtkSpinButton* arg0, GtkAdjustment* arg1)
{
  return gtk_spin_button_set_adjustment(arg0, arg1);
}

GtkAdjustment* Pure_gtk_spin_button_get_adjustment(GtkSpinButton* arg0)
{
  return gtk_spin_button_get_adjustment(arg0);
}

void Pure_gtk_spin_button_set_digits(GtkSpinButton* arg0, unsigned int arg1)
{
  return gtk_spin_button_set_digits(arg0, arg1);
}

unsigned int Pure_gtk_spin_button_get_digits(GtkSpinButton* arg0)
{
  return gtk_spin_button_get_digits(arg0);
}

void Pure_gtk_spin_button_set_increments(GtkSpinButton* arg0, double arg1, double arg2)
{
  return gtk_spin_button_set_increments(arg0, arg1, arg2);
}

void Pure_gtk_spin_button_get_increments(GtkSpinButton* arg0, double* arg1, double* arg2)
{
  return gtk_spin_button_get_increments(arg0, arg1, arg2);
}

void Pure_gtk_spin_button_set_range(GtkSpinButton* arg0, double arg1, double arg2)
{
  return gtk_spin_button_set_range(arg0, arg1, arg2);
}

void Pure_gtk_spin_button_get_range(GtkSpinButton* arg0, double* arg1, double* arg2)
{
  return gtk_spin_button_get_range(arg0, arg1, arg2);
}

double Pure_gtk_spin_button_get_value(GtkSpinButton* arg0)
{
  return gtk_spin_button_get_value(arg0);
}

int Pure_gtk_spin_button_get_value_as_int(GtkSpinButton* arg0)
{
  return gtk_spin_button_get_value_as_int(arg0);
}

void Pure_gtk_spin_button_set_value(GtkSpinButton* arg0, double arg1)
{
  return gtk_spin_button_set_value(arg0, arg1);
}

void Pure_gtk_spin_button_set_update_policy(GtkSpinButton* arg0, unsigned int arg1)
{
  return gtk_spin_button_set_update_policy(arg0, arg1);
}

unsigned int Pure_gtk_spin_button_get_update_policy(GtkSpinButton* arg0)
{
  return gtk_spin_button_get_update_policy(arg0);
}

void Pure_gtk_spin_button_set_numeric(GtkSpinButton* arg0, int arg1)
{
  return gtk_spin_button_set_numeric(arg0, arg1);
}

int Pure_gtk_spin_button_get_numeric(GtkSpinButton* arg0)
{
  return gtk_spin_button_get_numeric(arg0);
}

void Pure_gtk_spin_button_spin(GtkSpinButton* arg0, unsigned int arg1, double arg2)
{
  return gtk_spin_button_spin(arg0, arg1, arg2);
}

void Pure_gtk_spin_button_set_wrap(GtkSpinButton* arg0, int arg1)
{
  return gtk_spin_button_set_wrap(arg0, arg1);
}

int Pure_gtk_spin_button_get_wrap(GtkSpinButton* arg0)
{
  return gtk_spin_button_get_wrap(arg0);
}

void Pure_gtk_spin_button_set_snap_to_ticks(GtkSpinButton* arg0, int arg1)
{
  return gtk_spin_button_set_snap_to_ticks(arg0, arg1);
}

int Pure_gtk_spin_button_get_snap_to_ticks(GtkSpinButton* arg0)
{
  return gtk_spin_button_get_snap_to_ticks(arg0);
}

void Pure_gtk_spin_button_update(GtkSpinButton* arg0)
{
  return gtk_spin_button_update(arg0);
}

unsigned long Pure_gtk_statusbar_get_type()
{
  return gtk_statusbar_get_type();
}

GtkWidget* Pure_gtk_statusbar_new()
{
  return gtk_statusbar_new();
}

unsigned int Pure_gtk_statusbar_get_context_id(GtkStatusbar* arg0, char const* arg1)
{
  return gtk_statusbar_get_context_id(arg0, arg1);
}

unsigned int Pure_gtk_statusbar_push(GtkStatusbar* arg0, unsigned int arg1, char const* arg2)
{
  return gtk_statusbar_push(arg0, arg1, arg2);
}

void Pure_gtk_statusbar_pop(GtkStatusbar* arg0, unsigned int arg1)
{
  return gtk_statusbar_pop(arg0, arg1);
}

void Pure_gtk_statusbar_remove(GtkStatusbar* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_statusbar_remove(arg0, arg1, arg2);
}

void Pure_gtk_statusbar_set_has_resize_grip(GtkStatusbar* arg0, int arg1)
{
  return gtk_statusbar_set_has_resize_grip(arg0, arg1);
}

int Pure_gtk_statusbar_get_has_resize_grip(GtkStatusbar* arg0)
{
  return gtk_statusbar_get_has_resize_grip(arg0);
}

unsigned long Pure_gtk_status_icon_get_type()
{
  return gtk_status_icon_get_type();
}

GtkStatusIcon* Pure_gtk_status_icon_new()
{
  return gtk_status_icon_new();
}

GtkStatusIcon* Pure_gtk_status_icon_new_from_pixbuf(GdkPixbuf* arg0)
{
  return gtk_status_icon_new_from_pixbuf(arg0);
}

GtkStatusIcon* Pure_gtk_status_icon_new_from_file(char const* arg0)
{
  return gtk_status_icon_new_from_file(arg0);
}

GtkStatusIcon* Pure_gtk_status_icon_new_from_stock(char const* arg0)
{
  return gtk_status_icon_new_from_stock(arg0);
}

GtkStatusIcon* Pure_gtk_status_icon_new_from_icon_name(char const* arg0)
{
  return gtk_status_icon_new_from_icon_name(arg0);
}

GtkStatusIcon* Pure_gtk_status_icon_new_from_gicon(GIcon* arg0)
{
  return gtk_status_icon_new_from_gicon(arg0);
}

void Pure_gtk_status_icon_set_from_pixbuf(GtkStatusIcon* arg0, GdkPixbuf* arg1)
{
  return gtk_status_icon_set_from_pixbuf(arg0, arg1);
}

void Pure_gtk_status_icon_set_from_file(GtkStatusIcon* arg0, char const* arg1)
{
  return gtk_status_icon_set_from_file(arg0, arg1);
}

void Pure_gtk_status_icon_set_from_stock(GtkStatusIcon* arg0, char const* arg1)
{
  return gtk_status_icon_set_from_stock(arg0, arg1);
}

void Pure_gtk_status_icon_set_from_icon_name(GtkStatusIcon* arg0, char const* arg1)
{
  return gtk_status_icon_set_from_icon_name(arg0, arg1);
}

void Pure_gtk_status_icon_set_from_gicon(GtkStatusIcon* arg0, GIcon* arg1)
{
  return gtk_status_icon_set_from_gicon(arg0, arg1);
}

unsigned int Pure_gtk_status_icon_get_storage_type(GtkStatusIcon* arg0)
{
  return gtk_status_icon_get_storage_type(arg0);
}

GdkPixbuf* Pure_gtk_status_icon_get_pixbuf(GtkStatusIcon* arg0)
{
  return gtk_status_icon_get_pixbuf(arg0);
}

char const* Pure_gtk_status_icon_get_stock(GtkStatusIcon* arg0)
{
  return gtk_status_icon_get_stock(arg0);
}

char const* Pure_gtk_status_icon_get_icon_name(GtkStatusIcon* arg0)
{
  return gtk_status_icon_get_icon_name(arg0);
}

GIcon* Pure_gtk_status_icon_get_gicon(GtkStatusIcon* arg0)
{
  return gtk_status_icon_get_gicon(arg0);
}

int Pure_gtk_status_icon_get_size(GtkStatusIcon* arg0)
{
  return gtk_status_icon_get_size(arg0);
}

void Pure_gtk_status_icon_set_screen(GtkStatusIcon* arg0, GdkScreen* arg1)
{
  return gtk_status_icon_set_screen(arg0, arg1);
}

GdkScreen* Pure_gtk_status_icon_get_screen(GtkStatusIcon* arg0)
{
  return gtk_status_icon_get_screen(arg0);
}

void Pure_gtk_status_icon_set_tooltip(GtkStatusIcon* arg0, char const* arg1)
{
  return gtk_status_icon_set_tooltip(arg0, arg1);
}

void Pure_gtk_status_icon_set_visible(GtkStatusIcon* arg0, int arg1)
{
  return gtk_status_icon_set_visible(arg0, arg1);
}

int Pure_gtk_status_icon_get_visible(GtkStatusIcon* arg0)
{
  return gtk_status_icon_get_visible(arg0);
}

void Pure_gtk_status_icon_set_blinking(GtkStatusIcon* arg0, int arg1)
{
  return gtk_status_icon_set_blinking(arg0, arg1);
}

int Pure_gtk_status_icon_get_blinking(GtkStatusIcon* arg0)
{
  return gtk_status_icon_get_blinking(arg0);
}

int Pure_gtk_status_icon_is_embedded(GtkStatusIcon* arg0)
{
  return gtk_status_icon_is_embedded(arg0);
}

void Pure_gtk_status_icon_position_menu(GtkMenu* arg0, int* arg1, int* arg2, int* arg3, void* arg4)
{
  return gtk_status_icon_position_menu(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gtk_status_icon_get_geometry(GtkStatusIcon* arg0, GdkScreen** arg1, GdkRectangle* arg2, unsigned int* arg3)
{
  return gtk_status_icon_get_geometry(arg0, arg1, arg2, arg3);
}

unsigned int Pure_gtk_status_icon_get_x11_window_id(GtkStatusIcon* arg0)
{
  return gtk_status_icon_get_x11_window_id(arg0);
}

void Pure_gtk_stock_add(GtkStockItem const* arg0, unsigned int arg1)
{
  return gtk_stock_add(arg0, arg1);
}

void Pure_gtk_stock_add_static(GtkStockItem const* arg0, unsigned int arg1)
{
  return gtk_stock_add_static(arg0, arg1);
}

int Pure_gtk_stock_lookup(char const* arg0, GtkStockItem* arg1)
{
  return gtk_stock_lookup(arg0, arg1);
}

GSList* Pure_gtk_stock_list_ids()
{
  return gtk_stock_list_ids();
}

GtkStockItem* Pure_gtk_stock_item_copy(GtkStockItem const* arg0)
{
  return gtk_stock_item_copy(arg0);
}

void Pure_gtk_stock_item_free(GtkStockItem* arg0)
{
  return gtk_stock_item_free(arg0);
}

void Pure_gtk_stock_set_translate_func(char const* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_stock_set_translate_func(arg0, arg1, arg2, arg3);
}

unsigned long Pure_gtk_table_get_type()
{
  return gtk_table_get_type();
}

GtkWidget* Pure_gtk_table_new(unsigned int arg0, unsigned int arg1, int arg2)
{
  return gtk_table_new(arg0, arg1, arg2);
}

void Pure_gtk_table_resize(GtkTable* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_table_resize(arg0, arg1, arg2);
}

void Pure_gtk_table_attach(GtkTable* arg0, GtkWidget* arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6, unsigned int arg7, unsigned int arg8, unsigned int arg9)
{
  return gtk_table_attach(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_gtk_table_attach_defaults(GtkTable* arg0, GtkWidget* arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5)
{
  return gtk_table_attach_defaults(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_table_set_row_spacing(GtkTable* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_table_set_row_spacing(arg0, arg1, arg2);
}

unsigned int Pure_gtk_table_get_row_spacing(GtkTable* arg0, unsigned int arg1)
{
  return gtk_table_get_row_spacing(arg0, arg1);
}

void Pure_gtk_table_set_col_spacing(GtkTable* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_table_set_col_spacing(arg0, arg1, arg2);
}

unsigned int Pure_gtk_table_get_col_spacing(GtkTable* arg0, unsigned int arg1)
{
  return gtk_table_get_col_spacing(arg0, arg1);
}

void Pure_gtk_table_set_row_spacings(GtkTable* arg0, unsigned int arg1)
{
  return gtk_table_set_row_spacings(arg0, arg1);
}

unsigned int Pure_gtk_table_get_default_row_spacing(GtkTable* arg0)
{
  return gtk_table_get_default_row_spacing(arg0);
}

void Pure_gtk_table_set_col_spacings(GtkTable* arg0, unsigned int arg1)
{
  return gtk_table_set_col_spacings(arg0, arg1);
}

unsigned int Pure_gtk_table_get_default_col_spacing(GtkTable* arg0)
{
  return gtk_table_get_default_col_spacing(arg0);
}

void Pure_gtk_table_set_homogeneous(GtkTable* arg0, int arg1)
{
  return gtk_table_set_homogeneous(arg0, arg1);
}

int Pure_gtk_table_get_homogeneous(GtkTable* arg0)
{
  return gtk_table_get_homogeneous(arg0);
}

unsigned long Pure_gtk_tearoff_menu_item_get_type()
{
  return gtk_tearoff_menu_item_get_type();
}

GtkWidget* Pure_gtk_tearoff_menu_item_new()
{
  return gtk_tearoff_menu_item_new();
}

unsigned long Pure_gtk_text_tag_table_get_type()
{
  return gtk_text_tag_table_get_type();
}

GtkTextTagTable* Pure_gtk_text_tag_table_new()
{
  return gtk_text_tag_table_new();
}

void Pure_gtk_text_tag_table_add(GtkTextTagTable* arg0, GtkTextTag* arg1)
{
  return gtk_text_tag_table_add(arg0, arg1);
}

void Pure_gtk_text_tag_table_remove(GtkTextTagTable* arg0, GtkTextTag* arg1)
{
  return gtk_text_tag_table_remove(arg0, arg1);
}

GtkTextTag* Pure_gtk_text_tag_table_lookup(GtkTextTagTable* arg0, char const* arg1)
{
  return gtk_text_tag_table_lookup(arg0, arg1);
}

void Pure_gtk_text_tag_table_foreach(GtkTextTagTable* arg0, void* arg1, void* arg2)
{
  return gtk_text_tag_table_foreach(arg0, arg1, arg2);
}

int Pure_gtk_text_tag_table_get_size(GtkTextTagTable* arg0)
{
  return gtk_text_tag_table_get_size(arg0);
}

unsigned long Pure_gtk_text_mark_get_type()
{
  return gtk_text_mark_get_type();
}

void Pure_gtk_text_mark_set_visible(GtkTextMark* arg0, int arg1)
{
  return gtk_text_mark_set_visible(arg0, arg1);
}

int Pure_gtk_text_mark_get_visible(GtkTextMark* arg0)
{
  return gtk_text_mark_get_visible(arg0);
}

GtkTextMark* Pure_gtk_text_mark_new(char const* arg0, int arg1)
{
  return gtk_text_mark_new(arg0, arg1);
}

char const* Pure_gtk_text_mark_get_name(GtkTextMark* arg0)
{
  return gtk_text_mark_get_name(arg0);
}

int Pure_gtk_text_mark_get_deleted(GtkTextMark* arg0)
{
  return gtk_text_mark_get_deleted(arg0);
}

GtkTextBuffer* Pure_gtk_text_mark_get_buffer(GtkTextMark* arg0)
{
  return gtk_text_mark_get_buffer(arg0);
}

int Pure_gtk_text_mark_get_left_gravity(GtkTextMark* arg0)
{
  return gtk_text_mark_get_left_gravity(arg0);
}

unsigned long Pure_gtk_text_buffer_get_type()
{
  return gtk_text_buffer_get_type();
}

GtkTextBuffer* Pure_gtk_text_buffer_new(GtkTextTagTable* arg0)
{
  return gtk_text_buffer_new(arg0);
}

int Pure_gtk_text_buffer_get_line_count(GtkTextBuffer* arg0)
{
  return gtk_text_buffer_get_line_count(arg0);
}

int Pure_gtk_text_buffer_get_char_count(GtkTextBuffer* arg0)
{
  return gtk_text_buffer_get_char_count(arg0);
}

GtkTextTagTable* Pure_gtk_text_buffer_get_tag_table(GtkTextBuffer* arg0)
{
  return gtk_text_buffer_get_tag_table(arg0);
}

void Pure_gtk_text_buffer_set_text(GtkTextBuffer* arg0, char const* arg1, int arg2)
{
  return gtk_text_buffer_set_text(arg0, arg1, arg2);
}

void Pure_gtk_text_buffer_insert(GtkTextBuffer* arg0, GtkTextIter* arg1, char const* arg2, int arg3)
{
  return gtk_text_buffer_insert(arg0, arg1, arg2, arg3);
}

void Pure_gtk_text_buffer_insert_at_cursor(GtkTextBuffer* arg0, char const* arg1, int arg2)
{
  return gtk_text_buffer_insert_at_cursor(arg0, arg1, arg2);
}

int Pure_gtk_text_buffer_insert_interactive(GtkTextBuffer* arg0, GtkTextIter* arg1, char const* arg2, int arg3, int arg4)
{
  return gtk_text_buffer_insert_interactive(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gtk_text_buffer_insert_interactive_at_cursor(GtkTextBuffer* arg0, char const* arg1, int arg2, int arg3)
{
  return gtk_text_buffer_insert_interactive_at_cursor(arg0, arg1, arg2, arg3);
}

void Pure_gtk_text_buffer_insert_range(GtkTextBuffer* arg0, GtkTextIter* arg1, GtkTextIter const* arg2, GtkTextIter const* arg3)
{
  return gtk_text_buffer_insert_range(arg0, arg1, arg2, arg3);
}

int Pure_gtk_text_buffer_insert_range_interactive(GtkTextBuffer* arg0, GtkTextIter* arg1, GtkTextIter const* arg2, GtkTextIter const* arg3, int arg4)
{
  return gtk_text_buffer_insert_range_interactive(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_text_buffer_insert_with_tags(GtkTextBuffer* arg0, GtkTextIter* arg1, char const* arg2, int arg3, GtkTextTag* arg4)
{
  return gtk_text_buffer_insert_with_tags(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_text_buffer_insert_with_tags_by_name(GtkTextBuffer* arg0, GtkTextIter* arg1, char const* arg2, int arg3, char const* arg4)
{
  return gtk_text_buffer_insert_with_tags_by_name(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_text_buffer_delete(GtkTextBuffer* arg0, GtkTextIter* arg1, GtkTextIter* arg2)
{
  return gtk_text_buffer_delete(arg0, arg1, arg2);
}

int Pure_gtk_text_buffer_delete_interactive(GtkTextBuffer* arg0, GtkTextIter* arg1, GtkTextIter* arg2, int arg3)
{
  return gtk_text_buffer_delete_interactive(arg0, arg1, arg2, arg3);
}

int Pure_gtk_text_buffer_backspace(GtkTextBuffer* arg0, GtkTextIter* arg1, int arg2, int arg3)
{
  return gtk_text_buffer_backspace(arg0, arg1, arg2, arg3);
}

char* Pure_gtk_text_buffer_get_text(GtkTextBuffer* arg0, GtkTextIter const* arg1, GtkTextIter const* arg2, int arg3)
{
  return gtk_text_buffer_get_text(arg0, arg1, arg2, arg3);
}

char* Pure_gtk_text_buffer_get_slice(GtkTextBuffer* arg0, GtkTextIter const* arg1, GtkTextIter const* arg2, int arg3)
{
  return gtk_text_buffer_get_slice(arg0, arg1, arg2, arg3);
}

void Pure_gtk_text_buffer_insert_pixbuf(GtkTextBuffer* arg0, GtkTextIter* arg1, GdkPixbuf* arg2)
{
  return gtk_text_buffer_insert_pixbuf(arg0, arg1, arg2);
}

void Pure_gtk_text_buffer_insert_child_anchor(GtkTextBuffer* arg0, GtkTextIter* arg1, GtkTextChildAnchor* arg2)
{
  return gtk_text_buffer_insert_child_anchor(arg0, arg1, arg2);
}

GtkTextChildAnchor* Pure_gtk_text_buffer_create_child_anchor(GtkTextBuffer* arg0, GtkTextIter* arg1)
{
  return gtk_text_buffer_create_child_anchor(arg0, arg1);
}

void Pure_gtk_text_buffer_add_mark(GtkTextBuffer* arg0, GtkTextMark* arg1, GtkTextIter const* arg2)
{
  return gtk_text_buffer_add_mark(arg0, arg1, arg2);
}

GtkTextMark* Pure_gtk_text_buffer_create_mark(GtkTextBuffer* arg0, char const* arg1, GtkTextIter const* arg2, int arg3)
{
  return gtk_text_buffer_create_mark(arg0, arg1, arg2, arg3);
}

void Pure_gtk_text_buffer_move_mark(GtkTextBuffer* arg0, GtkTextMark* arg1, GtkTextIter const* arg2)
{
  return gtk_text_buffer_move_mark(arg0, arg1, arg2);
}

void Pure_gtk_text_buffer_delete_mark(GtkTextBuffer* arg0, GtkTextMark* arg1)
{
  return gtk_text_buffer_delete_mark(arg0, arg1);
}

GtkTextMark* Pure_gtk_text_buffer_get_mark(GtkTextBuffer* arg0, char const* arg1)
{
  return gtk_text_buffer_get_mark(arg0, arg1);
}

void Pure_gtk_text_buffer_move_mark_by_name(GtkTextBuffer* arg0, char const* arg1, GtkTextIter const* arg2)
{
  return gtk_text_buffer_move_mark_by_name(arg0, arg1, arg2);
}

void Pure_gtk_text_buffer_delete_mark_by_name(GtkTextBuffer* arg0, char const* arg1)
{
  return gtk_text_buffer_delete_mark_by_name(arg0, arg1);
}

GtkTextMark* Pure_gtk_text_buffer_get_insert(GtkTextBuffer* arg0)
{
  return gtk_text_buffer_get_insert(arg0);
}

GtkTextMark* Pure_gtk_text_buffer_get_selection_bound(GtkTextBuffer* arg0)
{
  return gtk_text_buffer_get_selection_bound(arg0);
}

void Pure_gtk_text_buffer_place_cursor(GtkTextBuffer* arg0, GtkTextIter const* arg1)
{
  return gtk_text_buffer_place_cursor(arg0, arg1);
}

void Pure_gtk_text_buffer_select_range(GtkTextBuffer* arg0, GtkTextIter const* arg1, GtkTextIter const* arg2)
{
  return gtk_text_buffer_select_range(arg0, arg1, arg2);
}

void Pure_gtk_text_buffer_apply_tag(GtkTextBuffer* arg0, GtkTextTag* arg1, GtkTextIter const* arg2, GtkTextIter const* arg3)
{
  return gtk_text_buffer_apply_tag(arg0, arg1, arg2, arg3);
}

void Pure_gtk_text_buffer_remove_tag(GtkTextBuffer* arg0, GtkTextTag* arg1, GtkTextIter const* arg2, GtkTextIter const* arg3)
{
  return gtk_text_buffer_remove_tag(arg0, arg1, arg2, arg3);
}

void Pure_gtk_text_buffer_apply_tag_by_name(GtkTextBuffer* arg0, char const* arg1, GtkTextIter const* arg2, GtkTextIter const* arg3)
{
  return gtk_text_buffer_apply_tag_by_name(arg0, arg1, arg2, arg3);
}

void Pure_gtk_text_buffer_remove_tag_by_name(GtkTextBuffer* arg0, char const* arg1, GtkTextIter const* arg2, GtkTextIter const* arg3)
{
  return gtk_text_buffer_remove_tag_by_name(arg0, arg1, arg2, arg3);
}

void Pure_gtk_text_buffer_remove_all_tags(GtkTextBuffer* arg0, GtkTextIter const* arg1, GtkTextIter const* arg2)
{
  return gtk_text_buffer_remove_all_tags(arg0, arg1, arg2);
}

GtkTextTag* Pure_gtk_text_buffer_create_tag(GtkTextBuffer* arg0, char const* arg1, char const* arg2)
{
  return gtk_text_buffer_create_tag(arg0, arg1, arg2);
}

void Pure_gtk_text_buffer_get_iter_at_line_offset(GtkTextBuffer* arg0, GtkTextIter* arg1, int arg2, int arg3)
{
  return gtk_text_buffer_get_iter_at_line_offset(arg0, arg1, arg2, arg3);
}

void Pure_gtk_text_buffer_get_iter_at_line_index(GtkTextBuffer* arg0, GtkTextIter* arg1, int arg2, int arg3)
{
  return gtk_text_buffer_get_iter_at_line_index(arg0, arg1, arg2, arg3);
}

void Pure_gtk_text_buffer_get_iter_at_offset(GtkTextBuffer* arg0, GtkTextIter* arg1, int arg2)
{
  return gtk_text_buffer_get_iter_at_offset(arg0, arg1, arg2);
}

void Pure_gtk_text_buffer_get_iter_at_line(GtkTextBuffer* arg0, GtkTextIter* arg1, int arg2)
{
  return gtk_text_buffer_get_iter_at_line(arg0, arg1, arg2);
}

void Pure_gtk_text_buffer_get_start_iter(GtkTextBuffer* arg0, GtkTextIter* arg1)
{
  return gtk_text_buffer_get_start_iter(arg0, arg1);
}

void Pure_gtk_text_buffer_get_end_iter(GtkTextBuffer* arg0, GtkTextIter* arg1)
{
  return gtk_text_buffer_get_end_iter(arg0, arg1);
}

void Pure_gtk_text_buffer_get_bounds(GtkTextBuffer* arg0, GtkTextIter* arg1, GtkTextIter* arg2)
{
  return gtk_text_buffer_get_bounds(arg0, arg1, arg2);
}

void Pure_gtk_text_buffer_get_iter_at_mark(GtkTextBuffer* arg0, GtkTextIter* arg1, GtkTextMark* arg2)
{
  return gtk_text_buffer_get_iter_at_mark(arg0, arg1, arg2);
}

void Pure_gtk_text_buffer_get_iter_at_child_anchor(GtkTextBuffer* arg0, GtkTextIter* arg1, GtkTextChildAnchor* arg2)
{
  return gtk_text_buffer_get_iter_at_child_anchor(arg0, arg1, arg2);
}

int Pure_gtk_text_buffer_get_modified(GtkTextBuffer* arg0)
{
  return gtk_text_buffer_get_modified(arg0);
}

void Pure_gtk_text_buffer_set_modified(GtkTextBuffer* arg0, int arg1)
{
  return gtk_text_buffer_set_modified(arg0, arg1);
}

int Pure_gtk_text_buffer_get_has_selection(GtkTextBuffer* arg0)
{
  return gtk_text_buffer_get_has_selection(arg0);
}

void Pure_gtk_text_buffer_add_selection_clipboard(GtkTextBuffer* arg0, GtkClipboard* arg1)
{
  return gtk_text_buffer_add_selection_clipboard(arg0, arg1);
}

void Pure_gtk_text_buffer_remove_selection_clipboard(GtkTextBuffer* arg0, GtkClipboard* arg1)
{
  return gtk_text_buffer_remove_selection_clipboard(arg0, arg1);
}

void Pure_gtk_text_buffer_cut_clipboard(GtkTextBuffer* arg0, GtkClipboard* arg1, int arg2)
{
  return gtk_text_buffer_cut_clipboard(arg0, arg1, arg2);
}

void Pure_gtk_text_buffer_copy_clipboard(GtkTextBuffer* arg0, GtkClipboard* arg1)
{
  return gtk_text_buffer_copy_clipboard(arg0, arg1);
}

void Pure_gtk_text_buffer_paste_clipboard(GtkTextBuffer* arg0, GtkClipboard* arg1, GtkTextIter* arg2, int arg3)
{
  return gtk_text_buffer_paste_clipboard(arg0, arg1, arg2, arg3);
}

int Pure_gtk_text_buffer_get_selection_bounds(GtkTextBuffer* arg0, GtkTextIter* arg1, GtkTextIter* arg2)
{
  return gtk_text_buffer_get_selection_bounds(arg0, arg1, arg2);
}

int Pure_gtk_text_buffer_delete_selection(GtkTextBuffer* arg0, int arg1, int arg2)
{
  return gtk_text_buffer_delete_selection(arg0, arg1, arg2);
}

void Pure_gtk_text_buffer_begin_user_action(GtkTextBuffer* arg0)
{
  return gtk_text_buffer_begin_user_action(arg0);
}

void Pure_gtk_text_buffer_end_user_action(GtkTextBuffer* arg0)
{
  return gtk_text_buffer_end_user_action(arg0);
}

GtkTargetList* Pure_gtk_text_buffer_get_copy_target_list(GtkTextBuffer* arg0)
{
  return gtk_text_buffer_get_copy_target_list(arg0);
}

GtkTargetList* Pure_gtk_text_buffer_get_paste_target_list(GtkTextBuffer* arg0)
{
  return gtk_text_buffer_get_paste_target_list(arg0);
}

struct _GdkAtom* Pure_gtk_text_buffer_register_serialize_format(GtkTextBuffer* arg0, char const* arg1, void* arg2, void* arg3, void* arg4)
{
  return gtk_text_buffer_register_serialize_format(arg0, arg1, arg2, arg3, arg4);
}

struct _GdkAtom* Pure_gtk_text_buffer_register_serialize_tagset(GtkTextBuffer* arg0, char const* arg1)
{
  return gtk_text_buffer_register_serialize_tagset(arg0, arg1);
}

struct _GdkAtom* Pure_gtk_text_buffer_register_deserialize_format(GtkTextBuffer* arg0, char const* arg1, void* arg2, void* arg3, void* arg4)
{
  return gtk_text_buffer_register_deserialize_format(arg0, arg1, arg2, arg3, arg4);
}

struct _GdkAtom* Pure_gtk_text_buffer_register_deserialize_tagset(GtkTextBuffer* arg0, char const* arg1)
{
  return gtk_text_buffer_register_deserialize_tagset(arg0, arg1);
}

void Pure_gtk_text_buffer_unregister_serialize_format(GtkTextBuffer* arg0, struct _GdkAtom* arg1)
{
  return gtk_text_buffer_unregister_serialize_format(arg0, arg1);
}

void Pure_gtk_text_buffer_unregister_deserialize_format(GtkTextBuffer* arg0, struct _GdkAtom* arg1)
{
  return gtk_text_buffer_unregister_deserialize_format(arg0, arg1);
}

void Pure_gtk_text_buffer_deserialize_set_can_create_tags(GtkTextBuffer* arg0, struct _GdkAtom* arg1, int arg2)
{
  return gtk_text_buffer_deserialize_set_can_create_tags(arg0, arg1, arg2);
}

int Pure_gtk_text_buffer_deserialize_get_can_create_tags(GtkTextBuffer* arg0, struct _GdkAtom* arg1)
{
  return gtk_text_buffer_deserialize_get_can_create_tags(arg0, arg1);
}

struct _GdkAtom** Pure_gtk_text_buffer_get_serialize_formats(GtkTextBuffer* arg0, int* arg1)
{
  return gtk_text_buffer_get_serialize_formats(arg0, arg1);
}

struct _GdkAtom** Pure_gtk_text_buffer_get_deserialize_formats(GtkTextBuffer* arg0, int* arg1)
{
  return gtk_text_buffer_get_deserialize_formats(arg0, arg1);
}

unsigned char* Pure_gtk_text_buffer_serialize(GtkTextBuffer* arg0, GtkTextBuffer* arg1, struct _GdkAtom* arg2, GtkTextIter const* arg3, GtkTextIter const* arg4, unsigned long* arg5)
{
  return gtk_text_buffer_serialize(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_gtk_text_buffer_deserialize(GtkTextBuffer* arg0, GtkTextBuffer* arg1, struct _GdkAtom* arg2, GtkTextIter* arg3, unsigned char const* arg4, unsigned long arg5, GError** arg6)
{
  return gtk_text_buffer_deserialize(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

unsigned long Pure_gtk_text_view_get_type()
{
  return gtk_text_view_get_type();
}

GtkWidget* Pure_gtk_text_view_new()
{
  return gtk_text_view_new();
}

GtkWidget* Pure_gtk_text_view_new_with_buffer(GtkTextBuffer* arg0)
{
  return gtk_text_view_new_with_buffer(arg0);
}

void Pure_gtk_text_view_set_buffer(GtkTextView* arg0, GtkTextBuffer* arg1)
{
  return gtk_text_view_set_buffer(arg0, arg1);
}

GtkTextBuffer* Pure_gtk_text_view_get_buffer(GtkTextView* arg0)
{
  return gtk_text_view_get_buffer(arg0);
}

int Pure_gtk_text_view_scroll_to_iter(GtkTextView* arg0, GtkTextIter* arg1, double arg2, int arg3, double arg4, double arg5)
{
  return gtk_text_view_scroll_to_iter(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_text_view_scroll_to_mark(GtkTextView* arg0, GtkTextMark* arg1, double arg2, int arg3, double arg4, double arg5)
{
  return gtk_text_view_scroll_to_mark(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_text_view_scroll_mark_onscreen(GtkTextView* arg0, GtkTextMark* arg1)
{
  return gtk_text_view_scroll_mark_onscreen(arg0, arg1);
}

int Pure_gtk_text_view_move_mark_onscreen(GtkTextView* arg0, GtkTextMark* arg1)
{
  return gtk_text_view_move_mark_onscreen(arg0, arg1);
}

int Pure_gtk_text_view_place_cursor_onscreen(GtkTextView* arg0)
{
  return gtk_text_view_place_cursor_onscreen(arg0);
}

void Pure_gtk_text_view_get_visible_rect(GtkTextView* arg0, GdkRectangle* arg1)
{
  return gtk_text_view_get_visible_rect(arg0, arg1);
}

void Pure_gtk_text_view_set_cursor_visible(GtkTextView* arg0, int arg1)
{
  return gtk_text_view_set_cursor_visible(arg0, arg1);
}

int Pure_gtk_text_view_get_cursor_visible(GtkTextView* arg0)
{
  return gtk_text_view_get_cursor_visible(arg0);
}

void Pure_gtk_text_view_get_iter_location(GtkTextView* arg0, GtkTextIter const* arg1, GdkRectangle* arg2)
{
  return gtk_text_view_get_iter_location(arg0, arg1, arg2);
}

void Pure_gtk_text_view_get_iter_at_location(GtkTextView* arg0, GtkTextIter* arg1, int arg2, int arg3)
{
  return gtk_text_view_get_iter_at_location(arg0, arg1, arg2, arg3);
}

void Pure_gtk_text_view_get_iter_at_position(GtkTextView* arg0, GtkTextIter* arg1, int* arg2, int arg3, int arg4)
{
  return gtk_text_view_get_iter_at_position(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_text_view_get_line_yrange(GtkTextView* arg0, GtkTextIter const* arg1, int* arg2, int* arg3)
{
  return gtk_text_view_get_line_yrange(arg0, arg1, arg2, arg3);
}

void Pure_gtk_text_view_get_line_at_y(GtkTextView* arg0, GtkTextIter* arg1, int arg2, int* arg3)
{
  return gtk_text_view_get_line_at_y(arg0, arg1, arg2, arg3);
}

void Pure_gtk_text_view_buffer_to_window_coords(GtkTextView* arg0, unsigned int arg1, int arg2, int arg3, int* arg4, int* arg5)
{
  return gtk_text_view_buffer_to_window_coords(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_text_view_window_to_buffer_coords(GtkTextView* arg0, unsigned int arg1, int arg2, int arg3, int* arg4, int* arg5)
{
  return gtk_text_view_window_to_buffer_coords(arg0, arg1, arg2, arg3, arg4, arg5);
}

GdkWindow* Pure_gtk_text_view_get_window(GtkTextView* arg0, unsigned int arg1)
{
  return gtk_text_view_get_window(arg0, arg1);
}

unsigned int Pure_gtk_text_view_get_window_type(GtkTextView* arg0, GdkWindow* arg1)
{
  return gtk_text_view_get_window_type(arg0, arg1);
}

void Pure_gtk_text_view_set_border_window_size(GtkTextView* arg0, unsigned int arg1, int arg2)
{
  return gtk_text_view_set_border_window_size(arg0, arg1, arg2);
}

int Pure_gtk_text_view_get_border_window_size(GtkTextView* arg0, unsigned int arg1)
{
  return gtk_text_view_get_border_window_size(arg0, arg1);
}

int Pure_gtk_text_view_forward_display_line(GtkTextView* arg0, GtkTextIter* arg1)
{
  return gtk_text_view_forward_display_line(arg0, arg1);
}

int Pure_gtk_text_view_backward_display_line(GtkTextView* arg0, GtkTextIter* arg1)
{
  return gtk_text_view_backward_display_line(arg0, arg1);
}

int Pure_gtk_text_view_forward_display_line_end(GtkTextView* arg0, GtkTextIter* arg1)
{
  return gtk_text_view_forward_display_line_end(arg0, arg1);
}

int Pure_gtk_text_view_backward_display_line_start(GtkTextView* arg0, GtkTextIter* arg1)
{
  return gtk_text_view_backward_display_line_start(arg0, arg1);
}

int Pure_gtk_text_view_starts_display_line(GtkTextView* arg0, GtkTextIter const* arg1)
{
  return gtk_text_view_starts_display_line(arg0, arg1);
}

int Pure_gtk_text_view_move_visually(GtkTextView* arg0, GtkTextIter* arg1, int arg2)
{
  return gtk_text_view_move_visually(arg0, arg1, arg2);
}

void Pure_gtk_text_view_add_child_at_anchor(GtkTextView* arg0, GtkWidget* arg1, GtkTextChildAnchor* arg2)
{
  return gtk_text_view_add_child_at_anchor(arg0, arg1, arg2);
}

void Pure_gtk_text_view_add_child_in_window(GtkTextView* arg0, GtkWidget* arg1, unsigned int arg2, int arg3, int arg4)
{
  return gtk_text_view_add_child_in_window(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_text_view_move_child(GtkTextView* arg0, GtkWidget* arg1, int arg2, int arg3)
{
  return gtk_text_view_move_child(arg0, arg1, arg2, arg3);
}

void Pure_gtk_text_view_set_wrap_mode(GtkTextView* arg0, unsigned int arg1)
{
  return gtk_text_view_set_wrap_mode(arg0, arg1);
}

unsigned int Pure_gtk_text_view_get_wrap_mode(GtkTextView* arg0)
{
  return gtk_text_view_get_wrap_mode(arg0);
}

void Pure_gtk_text_view_set_editable(GtkTextView* arg0, int arg1)
{
  return gtk_text_view_set_editable(arg0, arg1);
}

int Pure_gtk_text_view_get_editable(GtkTextView* arg0)
{
  return gtk_text_view_get_editable(arg0);
}

void Pure_gtk_text_view_set_overwrite(GtkTextView* arg0, int arg1)
{
  return gtk_text_view_set_overwrite(arg0, arg1);
}

int Pure_gtk_text_view_get_overwrite(GtkTextView* arg0)
{
  return gtk_text_view_get_overwrite(arg0);
}

void Pure_gtk_text_view_set_accepts_tab(GtkTextView* arg0, int arg1)
{
  return gtk_text_view_set_accepts_tab(arg0, arg1);
}

int Pure_gtk_text_view_get_accepts_tab(GtkTextView* arg0)
{
  return gtk_text_view_get_accepts_tab(arg0);
}

void Pure_gtk_text_view_set_pixels_above_lines(GtkTextView* arg0, int arg1)
{
  return gtk_text_view_set_pixels_above_lines(arg0, arg1);
}

int Pure_gtk_text_view_get_pixels_above_lines(GtkTextView* arg0)
{
  return gtk_text_view_get_pixels_above_lines(arg0);
}

void Pure_gtk_text_view_set_pixels_below_lines(GtkTextView* arg0, int arg1)
{
  return gtk_text_view_set_pixels_below_lines(arg0, arg1);
}

int Pure_gtk_text_view_get_pixels_below_lines(GtkTextView* arg0)
{
  return gtk_text_view_get_pixels_below_lines(arg0);
}

void Pure_gtk_text_view_set_pixels_inside_wrap(GtkTextView* arg0, int arg1)
{
  return gtk_text_view_set_pixels_inside_wrap(arg0, arg1);
}

int Pure_gtk_text_view_get_pixels_inside_wrap(GtkTextView* arg0)
{
  return gtk_text_view_get_pixels_inside_wrap(arg0);
}

void Pure_gtk_text_view_set_justification(GtkTextView* arg0, unsigned int arg1)
{
  return gtk_text_view_set_justification(arg0, arg1);
}

unsigned int Pure_gtk_text_view_get_justification(GtkTextView* arg0)
{
  return gtk_text_view_get_justification(arg0);
}

void Pure_gtk_text_view_set_left_margin(GtkTextView* arg0, int arg1)
{
  return gtk_text_view_set_left_margin(arg0, arg1);
}

int Pure_gtk_text_view_get_left_margin(GtkTextView* arg0)
{
  return gtk_text_view_get_left_margin(arg0);
}

void Pure_gtk_text_view_set_right_margin(GtkTextView* arg0, int arg1)
{
  return gtk_text_view_set_right_margin(arg0, arg1);
}

int Pure_gtk_text_view_get_right_margin(GtkTextView* arg0)
{
  return gtk_text_view_get_right_margin(arg0);
}

void Pure_gtk_text_view_set_indent(GtkTextView* arg0, int arg1)
{
  return gtk_text_view_set_indent(arg0, arg1);
}

int Pure_gtk_text_view_get_indent(GtkTextView* arg0)
{
  return gtk_text_view_get_indent(arg0);
}

void Pure_gtk_text_view_set_tabs(GtkTextView* arg0, PangoTabArray* arg1)
{
  return gtk_text_view_set_tabs(arg0, arg1);
}

PangoTabArray* Pure_gtk_text_view_get_tabs(GtkTextView* arg0)
{
  return gtk_text_view_get_tabs(arg0);
}

GtkTextAttributes* Pure_gtk_text_view_get_default_attributes(GtkTextView* arg0)
{
  return gtk_text_view_get_default_attributes(arg0);
}

unsigned long Pure_gtk_pixmap_get_type()
{
  return gtk_pixmap_get_type();
}

GtkWidget* Pure_gtk_pixmap_new(GdkPixmap* arg0, GdkBitmap* arg1)
{
  return gtk_pixmap_new(arg0, arg1);
}

void Pure_gtk_pixmap_set(GtkPixmap* arg0, GdkPixmap* arg1, GdkBitmap* arg2)
{
  return gtk_pixmap_set(arg0, arg1, arg2);
}

void Pure_gtk_pixmap_get(GtkPixmap* arg0, GdkPixmap** arg1, GdkBitmap** arg2)
{
  return gtk_pixmap_get(arg0, arg1, arg2);
}

void Pure_gtk_pixmap_set_build_insensitive(GtkPixmap* arg0, int arg1)
{
  return gtk_pixmap_set_build_insensitive(arg0, arg1);
}

unsigned long Pure_gtk_toolbar_get_type()
{
  return gtk_toolbar_get_type();
}

GtkWidget* Pure_gtk_toolbar_new()
{
  return gtk_toolbar_new();
}

void Pure_gtk_toolbar_insert(GtkToolbar* arg0, GtkToolItem* arg1, int arg2)
{
  return gtk_toolbar_insert(arg0, arg1, arg2);
}

int Pure_gtk_toolbar_get_item_index(GtkToolbar* arg0, GtkToolItem* arg1)
{
  return gtk_toolbar_get_item_index(arg0, arg1);
}

int Pure_gtk_toolbar_get_n_items(GtkToolbar* arg0)
{
  return gtk_toolbar_get_n_items(arg0);
}

GtkToolItem* Pure_gtk_toolbar_get_nth_item(GtkToolbar* arg0, int arg1)
{
  return gtk_toolbar_get_nth_item(arg0, arg1);
}

int Pure_gtk_toolbar_get_show_arrow(GtkToolbar* arg0)
{
  return gtk_toolbar_get_show_arrow(arg0);
}

void Pure_gtk_toolbar_set_show_arrow(GtkToolbar* arg0, int arg1)
{
  return gtk_toolbar_set_show_arrow(arg0, arg1);
}

unsigned int Pure_gtk_toolbar_get_orientation(GtkToolbar* arg0)
{
  return gtk_toolbar_get_orientation(arg0);
}

void Pure_gtk_toolbar_set_orientation(GtkToolbar* arg0, unsigned int arg1)
{
  return gtk_toolbar_set_orientation(arg0, arg1);
}

int Pure_gtk_toolbar_get_tooltips(GtkToolbar* arg0)
{
  return gtk_toolbar_get_tooltips(arg0);
}

void Pure_gtk_toolbar_set_tooltips(GtkToolbar* arg0, int arg1)
{
  return gtk_toolbar_set_tooltips(arg0, arg1);
}

unsigned int Pure_gtk_toolbar_get_style(GtkToolbar* arg0)
{
  return gtk_toolbar_get_style(arg0);
}

void Pure_gtk_toolbar_set_style(GtkToolbar* arg0, unsigned int arg1)
{
  return gtk_toolbar_set_style(arg0, arg1);
}

void Pure_gtk_toolbar_unset_style(GtkToolbar* arg0)
{
  return gtk_toolbar_unset_style(arg0);
}

unsigned int Pure_gtk_toolbar_get_icon_size(GtkToolbar* arg0)
{
  return gtk_toolbar_get_icon_size(arg0);
}

unsigned int Pure_gtk_toolbar_get_relief_style(GtkToolbar* arg0)
{
  return gtk_toolbar_get_relief_style(arg0);
}

int Pure_gtk_toolbar_get_drop_index(GtkToolbar* arg0, int arg1, int arg2)
{
  return gtk_toolbar_get_drop_index(arg0, arg1, arg2);
}

void Pure_gtk_toolbar_set_drop_highlight_item(GtkToolbar* arg0, GtkToolItem* arg1, int arg2)
{
  return gtk_toolbar_set_drop_highlight_item(arg0, arg1, arg2);
}

void Pure_gtk_toolbar_set_icon_size(GtkToolbar* arg0, unsigned int arg1)
{
  return gtk_toolbar_set_icon_size(arg0, arg1);
}

void Pure_gtk_toolbar_unset_icon_size(GtkToolbar* arg0)
{
  return gtk_toolbar_unset_icon_size(arg0);
}

GtkWidget* Pure_gtk_toolbar_append_item(GtkToolbar* arg0, char const* arg1, char const* arg2, char const* arg3, GtkWidget* arg4, void* arg5, void* arg6)
{
  return gtk_toolbar_append_item(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GtkWidget* Pure_gtk_toolbar_prepend_item(GtkToolbar* arg0, char const* arg1, char const* arg2, char const* arg3, GtkWidget* arg4, void* arg5, void* arg6)
{
  return gtk_toolbar_prepend_item(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GtkWidget* Pure_gtk_toolbar_insert_item(GtkToolbar* arg0, char const* arg1, char const* arg2, char const* arg3, GtkWidget* arg4, void* arg5, void* arg6, int arg7)
{
  return gtk_toolbar_insert_item(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

GtkWidget* Pure_gtk_toolbar_insert_stock(GtkToolbar* arg0, char const* arg1, char const* arg2, char const* arg3, void* arg4, void* arg5, int arg6)
{
  return gtk_toolbar_insert_stock(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_toolbar_append_space(GtkToolbar* arg0)
{
  return gtk_toolbar_append_space(arg0);
}

void Pure_gtk_toolbar_prepend_space(GtkToolbar* arg0)
{
  return gtk_toolbar_prepend_space(arg0);
}

void Pure_gtk_toolbar_insert_space(GtkToolbar* arg0, int arg1)
{
  return gtk_toolbar_insert_space(arg0, arg1);
}

void Pure_gtk_toolbar_remove_space(GtkToolbar* arg0, int arg1)
{
  return gtk_toolbar_remove_space(arg0, arg1);
}

GtkWidget* Pure_gtk_toolbar_append_element(GtkToolbar* arg0, unsigned int arg1, GtkWidget* arg2, char const* arg3, char const* arg4, char const* arg5, GtkWidget* arg6, void* arg7, void* arg8)
{
  return gtk_toolbar_append_element(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

GtkWidget* Pure_gtk_toolbar_prepend_element(GtkToolbar* arg0, unsigned int arg1, GtkWidget* arg2, char const* arg3, char const* arg4, char const* arg5, GtkWidget* arg6, void* arg7, void* arg8)
{
  return gtk_toolbar_prepend_element(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

GtkWidget* Pure_gtk_toolbar_insert_element(GtkToolbar* arg0, unsigned int arg1, GtkWidget* arg2, char const* arg3, char const* arg4, char const* arg5, GtkWidget* arg6, void* arg7, void* arg8, int arg9)
{
  return gtk_toolbar_insert_element(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_gtk_toolbar_append_widget(GtkToolbar* arg0, GtkWidget* arg1, char const* arg2, char const* arg3)
{
  return gtk_toolbar_append_widget(arg0, arg1, arg2, arg3);
}

void Pure_gtk_toolbar_prepend_widget(GtkToolbar* arg0, GtkWidget* arg1, char const* arg2, char const* arg3)
{
  return gtk_toolbar_prepend_widget(arg0, arg1, arg2, arg3);
}

void Pure_gtk_toolbar_insert_widget(GtkToolbar* arg0, GtkWidget* arg1, char const* arg2, char const* arg3, int arg4)
{
  return gtk_toolbar_insert_widget(arg0, arg1, arg2, arg3, arg4);
}

unsigned long Pure_gtk_tool_shell_get_type()
{
  return gtk_tool_shell_get_type();
}

unsigned int Pure_gtk_tool_shell_get_icon_size(GtkToolShell* arg0)
{
  return gtk_tool_shell_get_icon_size(arg0);
}

unsigned int Pure_gtk_tool_shell_get_orientation(GtkToolShell* arg0)
{
  return gtk_tool_shell_get_orientation(arg0);
}

unsigned int Pure_gtk_tool_shell_get_style(GtkToolShell* arg0)
{
  return gtk_tool_shell_get_style(arg0);
}

unsigned int Pure_gtk_tool_shell_get_relief_style(GtkToolShell* arg0)
{
  return gtk_tool_shell_get_relief_style(arg0);
}

void Pure_gtk_tool_shell_rebuild_menu(GtkToolShell* arg0)
{
  return gtk_tool_shell_rebuild_menu(arg0);
}

void Pure_gtk_test_init(int* arg0, char*** arg1)
{
  return gtk_test_init(arg0, arg1);
}

void Pure_gtk_test_register_all_types()
{
  return gtk_test_register_all_types();
}

unsigned long const* Pure_gtk_test_list_all_types(unsigned int* arg0)
{
  return gtk_test_list_all_types(arg0);
}

GtkWidget* Pure_gtk_test_find_widget(GtkWidget* arg0, char const* arg1, unsigned long arg2)
{
  return gtk_test_find_widget(arg0, arg1, arg2);
}

GtkWidget* Pure_gtk_test_create_widget(unsigned long arg0, char const* arg1)
{
  return gtk_test_create_widget(arg0, arg1);
}

GtkWidget* Pure_gtk_test_create_simple_window(char const* arg0, char const* arg1)
{
  return gtk_test_create_simple_window(arg0, arg1);
}

GtkWidget* Pure_gtk_test_display_button_window(char const* arg0, char const* arg1)
{
  return gtk_test_display_button_window(arg0, arg1);
}

void Pure_gtk_test_slider_set_perc(GtkWidget* arg0, double arg1)
{
  return gtk_test_slider_set_perc(arg0, arg1);
}

double Pure_gtk_test_slider_get_value(GtkWidget* arg0)
{
  return gtk_test_slider_get_value(arg0);
}

int Pure_gtk_test_spin_button_click(GtkSpinButton* arg0, unsigned int arg1, int arg2)
{
  return gtk_test_spin_button_click(arg0, arg1, arg2);
}

int Pure_gtk_test_widget_click(GtkWidget* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_test_widget_click(arg0, arg1, arg2);
}

int Pure_gtk_test_widget_send_key(GtkWidget* arg0, unsigned int arg1, unsigned int arg2)
{
  return gtk_test_widget_send_key(arg0, arg1, arg2);
}

void Pure_gtk_test_text_set(GtkWidget* arg0, char const* arg1)
{
  return gtk_test_text_set(arg0, arg1);
}

char* Pure_gtk_test_text_get(GtkWidget* arg0)
{
  return gtk_test_text_get(arg0);
}

GtkWidget* Pure_gtk_test_find_sibling(GtkWidget* arg0, unsigned long arg1)
{
  return gtk_test_find_sibling(arg0, arg1);
}

GtkWidget* Pure_gtk_test_find_label(GtkWidget* arg0, char const* arg1)
{
  return gtk_test_find_label(arg0, arg1);
}

unsigned long Pure_gtk_tree_drag_source_get_type()
{
  return gtk_tree_drag_source_get_type();
}

int Pure_gtk_tree_drag_source_row_draggable(GtkTreeDragSource* arg0, GtkTreePath* arg1)
{
  return gtk_tree_drag_source_row_draggable(arg0, arg1);
}

int Pure_gtk_tree_drag_source_drag_data_delete(GtkTreeDragSource* arg0, GtkTreePath* arg1)
{
  return gtk_tree_drag_source_drag_data_delete(arg0, arg1);
}

int Pure_gtk_tree_drag_source_drag_data_get(GtkTreeDragSource* arg0, GtkTreePath* arg1, GtkSelectionData* arg2)
{
  return gtk_tree_drag_source_drag_data_get(arg0, arg1, arg2);
}

unsigned long Pure_gtk_tree_drag_dest_get_type()
{
  return gtk_tree_drag_dest_get_type();
}

int Pure_gtk_tree_drag_dest_drag_data_received(GtkTreeDragDest* arg0, GtkTreePath* arg1, GtkSelectionData* arg2)
{
  return gtk_tree_drag_dest_drag_data_received(arg0, arg1, arg2);
}

int Pure_gtk_tree_drag_dest_row_drop_possible(GtkTreeDragDest* arg0, GtkTreePath* arg1, GtkSelectionData* arg2)
{
  return gtk_tree_drag_dest_row_drop_possible(arg0, arg1, arg2);
}

int Pure_gtk_tree_set_row_drag_data(GtkSelectionData* arg0, GtkTreeModel* arg1, GtkTreePath* arg2)
{
  return gtk_tree_set_row_drag_data(arg0, arg1, arg2);
}

int Pure_gtk_tree_get_row_drag_data(GtkSelectionData* arg0, GtkTreeModel** arg1, GtkTreePath** arg2)
{
  return gtk_tree_get_row_drag_data(arg0, arg1, arg2);
}

unsigned long Pure_gtk_tree_model_sort_get_type()
{
  return gtk_tree_model_sort_get_type();
}

GtkTreeModel* Pure_gtk_tree_model_sort_new_with_model(GtkTreeModel* arg0)
{
  return gtk_tree_model_sort_new_with_model(arg0);
}

GtkTreeModel* Pure_gtk_tree_model_sort_get_model(GtkTreeModelSort* arg0)
{
  return gtk_tree_model_sort_get_model(arg0);
}

GtkTreePath* Pure_gtk_tree_model_sort_convert_child_path_to_path(GtkTreeModelSort* arg0, GtkTreePath* arg1)
{
  return gtk_tree_model_sort_convert_child_path_to_path(arg0, arg1);
}

int Pure_gtk_tree_model_sort_convert_child_iter_to_iter(GtkTreeModelSort* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_model_sort_convert_child_iter_to_iter(arg0, arg1, arg2);
}

GtkTreePath* Pure_gtk_tree_model_sort_convert_path_to_child_path(GtkTreeModelSort* arg0, GtkTreePath* arg1)
{
  return gtk_tree_model_sort_convert_path_to_child_path(arg0, arg1);
}

void Pure_gtk_tree_model_sort_convert_iter_to_child_iter(GtkTreeModelSort* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_model_sort_convert_iter_to_child_iter(arg0, arg1, arg2);
}

void Pure_gtk_tree_model_sort_reset_default_sort_func(GtkTreeModelSort* arg0)
{
  return gtk_tree_model_sort_reset_default_sort_func(arg0);
}

void Pure_gtk_tree_model_sort_clear_cache(GtkTreeModelSort* arg0)
{
  return gtk_tree_model_sort_clear_cache(arg0);
}

int Pure_gtk_tree_model_sort_iter_is_valid(GtkTreeModelSort* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_model_sort_iter_is_valid(arg0, arg1);
}

unsigned long Pure_gtk_tree_selection_get_type()
{
  return gtk_tree_selection_get_type();
}

void Pure_gtk_tree_selection_set_mode(GtkTreeSelection* arg0, unsigned int arg1)
{
  return gtk_tree_selection_set_mode(arg0, arg1);
}

unsigned int Pure_gtk_tree_selection_get_mode(GtkTreeSelection* arg0)
{
  return gtk_tree_selection_get_mode(arg0);
}

void Pure_gtk_tree_selection_set_select_function(GtkTreeSelection* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_tree_selection_set_select_function(arg0, arg1, arg2, arg3);
}

void* Pure_gtk_tree_selection_get_user_data(GtkTreeSelection* arg0)
{
  return gtk_tree_selection_get_user_data(arg0);
}

GtkTreeView* Pure_gtk_tree_selection_get_tree_view(GtkTreeSelection* arg0)
{
  return gtk_tree_selection_get_tree_view(arg0);
}

void* Pure_gtk_tree_selection_get_select_function(GtkTreeSelection* arg0)
{
  return gtk_tree_selection_get_select_function(arg0);
}

int Pure_gtk_tree_selection_get_selected(GtkTreeSelection* arg0, GtkTreeModel** arg1, GtkTreeIter* arg2)
{
  return gtk_tree_selection_get_selected(arg0, arg1, arg2);
}

GList* Pure_gtk_tree_selection_get_selected_rows(GtkTreeSelection* arg0, GtkTreeModel** arg1)
{
  return gtk_tree_selection_get_selected_rows(arg0, arg1);
}

int Pure_gtk_tree_selection_count_selected_rows(GtkTreeSelection* arg0)
{
  return gtk_tree_selection_count_selected_rows(arg0);
}

void Pure_gtk_tree_selection_selected_foreach(GtkTreeSelection* arg0, void* arg1, void* arg2)
{
  return gtk_tree_selection_selected_foreach(arg0, arg1, arg2);
}

void Pure_gtk_tree_selection_select_path(GtkTreeSelection* arg0, GtkTreePath* arg1)
{
  return gtk_tree_selection_select_path(arg0, arg1);
}

void Pure_gtk_tree_selection_unselect_path(GtkTreeSelection* arg0, GtkTreePath* arg1)
{
  return gtk_tree_selection_unselect_path(arg0, arg1);
}

void Pure_gtk_tree_selection_select_iter(GtkTreeSelection* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_selection_select_iter(arg0, arg1);
}

void Pure_gtk_tree_selection_unselect_iter(GtkTreeSelection* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_selection_unselect_iter(arg0, arg1);
}

int Pure_gtk_tree_selection_path_is_selected(GtkTreeSelection* arg0, GtkTreePath* arg1)
{
  return gtk_tree_selection_path_is_selected(arg0, arg1);
}

int Pure_gtk_tree_selection_iter_is_selected(GtkTreeSelection* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_selection_iter_is_selected(arg0, arg1);
}

void Pure_gtk_tree_selection_select_all(GtkTreeSelection* arg0)
{
  return gtk_tree_selection_select_all(arg0);
}

void Pure_gtk_tree_selection_unselect_all(GtkTreeSelection* arg0)
{
  return gtk_tree_selection_unselect_all(arg0);
}

void Pure_gtk_tree_selection_select_range(GtkTreeSelection* arg0, GtkTreePath* arg1, GtkTreePath* arg2)
{
  return gtk_tree_selection_select_range(arg0, arg1, arg2);
}

void Pure_gtk_tree_selection_unselect_range(GtkTreeSelection* arg0, GtkTreePath* arg1, GtkTreePath* arg2)
{
  return gtk_tree_selection_unselect_range(arg0, arg1, arg2);
}

unsigned long Pure_gtk_tree_store_get_type()
{
  return gtk_tree_store_get_type();
}

GtkTreeStore* Pure_gtk_tree_store_new(int arg0)
{
  return gtk_tree_store_new(arg0);
}

GtkTreeStore* Pure_gtk_tree_store_newv(int arg0, unsigned long* arg1)
{
  return gtk_tree_store_newv(arg0, arg1);
}

void Pure_gtk_tree_store_set_column_types(GtkTreeStore* arg0, int arg1, unsigned long* arg2)
{
  return gtk_tree_store_set_column_types(arg0, arg1, arg2);
}

void Pure_gtk_tree_store_set_value(GtkTreeStore* arg0, GtkTreeIter* arg1, int arg2, GValue* arg3)
{
  return gtk_tree_store_set_value(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_store_set(GtkTreeStore* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_store_set(arg0, arg1);
}

void Pure_gtk_tree_store_set_valuesv(GtkTreeStore* arg0, GtkTreeIter* arg1, int* arg2, GValue* arg3, int arg4)
{
  return gtk_tree_store_set_valuesv(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_tree_store_set_valist(GtkTreeStore* arg0, GtkTreeIter* arg1, void* arg2)
{
  return gtk_tree_store_set_valist(arg0, arg1, arg2);
}

int Pure_gtk_tree_store_remove(GtkTreeStore* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_store_remove(arg0, arg1);
}

void Pure_gtk_tree_store_insert(GtkTreeStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2, int arg3)
{
  return gtk_tree_store_insert(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_store_insert_before(GtkTreeStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2, GtkTreeIter* arg3)
{
  return gtk_tree_store_insert_before(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_store_insert_after(GtkTreeStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2, GtkTreeIter* arg3)
{
  return gtk_tree_store_insert_after(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_store_insert_with_values(GtkTreeStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2, int arg3)
{
  return gtk_tree_store_insert_with_values(arg0, arg1, arg2, arg3);
}

void Pure_gtk_tree_store_insert_with_valuesv(GtkTreeStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2, int arg3, int* arg4, GValue* arg5, int arg6)
{
  return gtk_tree_store_insert_with_valuesv(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_tree_store_prepend(GtkTreeStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_store_prepend(arg0, arg1, arg2);
}

void Pure_gtk_tree_store_append(GtkTreeStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_store_append(arg0, arg1, arg2);
}

int Pure_gtk_tree_store_is_ancestor(GtkTreeStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_store_is_ancestor(arg0, arg1, arg2);
}

int Pure_gtk_tree_store_iter_depth(GtkTreeStore* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_store_iter_depth(arg0, arg1);
}

void Pure_gtk_tree_store_clear(GtkTreeStore* arg0)
{
  return gtk_tree_store_clear(arg0);
}

int Pure_gtk_tree_store_iter_is_valid(GtkTreeStore* arg0, GtkTreeIter* arg1)
{
  return gtk_tree_store_iter_is_valid(arg0, arg1);
}

void Pure_gtk_tree_store_reorder(GtkTreeStore* arg0, GtkTreeIter* arg1, int* arg2)
{
  return gtk_tree_store_reorder(arg0, arg1, arg2);
}

void Pure_gtk_tree_store_swap(GtkTreeStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_store_swap(arg0, arg1, arg2);
}

void Pure_gtk_tree_store_move_before(GtkTreeStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_store_move_before(arg0, arg1, arg2);
}

void Pure_gtk_tree_store_move_after(GtkTreeStore* arg0, GtkTreeIter* arg1, GtkTreeIter* arg2)
{
  return gtk_tree_store_move_after(arg0, arg1, arg2);
}

unsigned long Pure_gtk_ui_manager_get_type()
{
  return gtk_ui_manager_get_type();
}

GtkUIManager* Pure_gtk_ui_manager_new()
{
  return gtk_ui_manager_new();
}

void Pure_gtk_ui_manager_set_add_tearoffs(GtkUIManager* arg0, int arg1)
{
  return gtk_ui_manager_set_add_tearoffs(arg0, arg1);
}

int Pure_gtk_ui_manager_get_add_tearoffs(GtkUIManager* arg0)
{
  return gtk_ui_manager_get_add_tearoffs(arg0);
}

void Pure_gtk_ui_manager_insert_action_group(GtkUIManager* arg0, GtkActionGroup* arg1, int arg2)
{
  return gtk_ui_manager_insert_action_group(arg0, arg1, arg2);
}

void Pure_gtk_ui_manager_remove_action_group(GtkUIManager* arg0, GtkActionGroup* arg1)
{
  return gtk_ui_manager_remove_action_group(arg0, arg1);
}

GList* Pure_gtk_ui_manager_get_action_groups(GtkUIManager* arg0)
{
  return gtk_ui_manager_get_action_groups(arg0);
}

GtkAccelGroup* Pure_gtk_ui_manager_get_accel_group(GtkUIManager* arg0)
{
  return gtk_ui_manager_get_accel_group(arg0);
}

GtkWidget* Pure_gtk_ui_manager_get_widget(GtkUIManager* arg0, char const* arg1)
{
  return gtk_ui_manager_get_widget(arg0, arg1);
}

GSList* Pure_gtk_ui_manager_get_toplevels(GtkUIManager* arg0, unsigned int arg1)
{
  return gtk_ui_manager_get_toplevels(arg0, arg1);
}

GtkAction* Pure_gtk_ui_manager_get_action(GtkUIManager* arg0, char const* arg1)
{
  return gtk_ui_manager_get_action(arg0, arg1);
}

unsigned int Pure_gtk_ui_manager_add_ui_from_string(GtkUIManager* arg0, char const* arg1, long arg2, GError** arg3)
{
  return gtk_ui_manager_add_ui_from_string(arg0, arg1, arg2, arg3);
}

unsigned int Pure_gtk_ui_manager_add_ui_from_file(GtkUIManager* arg0, char const* arg1, GError** arg2)
{
  return gtk_ui_manager_add_ui_from_file(arg0, arg1, arg2);
}

void Pure_gtk_ui_manager_add_ui(GtkUIManager* arg0, unsigned int arg1, char const* arg2, char const* arg3, char const* arg4, unsigned int arg5, int arg6)
{
  return gtk_ui_manager_add_ui(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_ui_manager_remove_ui(GtkUIManager* arg0, unsigned int arg1)
{
  return gtk_ui_manager_remove_ui(arg0, arg1);
}

char* Pure_gtk_ui_manager_get_ui(GtkUIManager* arg0)
{
  return gtk_ui_manager_get_ui(arg0);
}

void Pure_gtk_ui_manager_ensure_update(GtkUIManager* arg0)
{
  return gtk_ui_manager_ensure_update(arg0);
}

unsigned int Pure_gtk_ui_manager_new_merge_id(GtkUIManager* arg0)
{
  return gtk_ui_manager_new_merge_id(arg0);
}

unsigned long Pure_gtk_vbutton_box_get_type()
{
  return gtk_vbutton_box_get_type();
}

GtkWidget* Pure_gtk_vbutton_box_new()
{
  return gtk_vbutton_box_new();
}

int Pure_gtk_vbutton_box_get_spacing_default()
{
  return gtk_vbutton_box_get_spacing_default();
}

void Pure_gtk_vbutton_box_set_spacing_default(int arg0)
{
  return gtk_vbutton_box_set_spacing_default(arg0);
}

unsigned int Pure_gtk_vbutton_box_get_layout_default()
{
  return gtk_vbutton_box_get_layout_default();
}

void Pure_gtk_vbutton_box_set_layout_default(unsigned int arg0)
{
  return gtk_vbutton_box_set_layout_default(arg0);
}

unsigned long Pure_gtk_volume_button_get_type()
{
  return gtk_volume_button_get_type();
}

GtkWidget* Pure_gtk_volume_button_new()
{
  return gtk_volume_button_new();
}

unsigned long Pure_gtk_vpaned_get_type()
{
  return gtk_vpaned_get_type();
}

GtkWidget* Pure_gtk_vpaned_new()
{
  return gtk_vpaned_new();
}

unsigned long Pure_gtk_vruler_get_type()
{
  return gtk_vruler_get_type();
}

GtkWidget* Pure_gtk_vruler_new()
{
  return gtk_vruler_new();
}

unsigned long Pure_gtk_vscale_get_type()
{
  return gtk_vscale_get_type();
}

GtkWidget* Pure_gtk_vscale_new(GtkAdjustment* arg0)
{
  return gtk_vscale_new(arg0);
}

GtkWidget* Pure_gtk_vscale_new_with_range(double arg0, double arg1, double arg2)
{
  return gtk_vscale_new_with_range(arg0, arg1, arg2);
}

unsigned long Pure_gtk_vseparator_get_type()
{
  return gtk_vseparator_get_type();
}

GtkWidget* Pure_gtk_vseparator_new()
{
  return gtk_vseparator_new();
}

unsigned long Pure_gtk_clist_get_type()
{
  return gtk_clist_get_type();
}

GtkWidget* Pure_gtk_clist_new(int arg0)
{
  return gtk_clist_new(arg0);
}

GtkWidget* Pure_gtk_clist_new_with_titles(int arg0, char** arg1)
{
  return gtk_clist_new_with_titles(arg0, arg1);
}

void Pure_gtk_clist_set_hadjustment(GtkCList* arg0, GtkAdjustment* arg1)
{
  return gtk_clist_set_hadjustment(arg0, arg1);
}

void Pure_gtk_clist_set_vadjustment(GtkCList* arg0, GtkAdjustment* arg1)
{
  return gtk_clist_set_vadjustment(arg0, arg1);
}

GtkAdjustment* Pure_gtk_clist_get_hadjustment(GtkCList* arg0)
{
  return gtk_clist_get_hadjustment(arg0);
}

GtkAdjustment* Pure_gtk_clist_get_vadjustment(GtkCList* arg0)
{
  return gtk_clist_get_vadjustment(arg0);
}

void Pure_gtk_clist_set_shadow_type(GtkCList* arg0, unsigned int arg1)
{
  return gtk_clist_set_shadow_type(arg0, arg1);
}

void Pure_gtk_clist_set_selection_mode(GtkCList* arg0, unsigned int arg1)
{
  return gtk_clist_set_selection_mode(arg0, arg1);
}

void Pure_gtk_clist_set_reorderable(GtkCList* arg0, int arg1)
{
  return gtk_clist_set_reorderable(arg0, arg1);
}

void Pure_gtk_clist_set_use_drag_icons(GtkCList* arg0, int arg1)
{
  return gtk_clist_set_use_drag_icons(arg0, arg1);
}

void Pure_gtk_clist_set_button_actions(GtkCList* arg0, unsigned int arg1, unsigned char arg2)
{
  return gtk_clist_set_button_actions(arg0, arg1, arg2);
}

void Pure_gtk_clist_freeze(GtkCList* arg0)
{
  return gtk_clist_freeze(arg0);
}

void Pure_gtk_clist_thaw(GtkCList* arg0)
{
  return gtk_clist_thaw(arg0);
}

void Pure_gtk_clist_column_titles_show(GtkCList* arg0)
{
  return gtk_clist_column_titles_show(arg0);
}

void Pure_gtk_clist_column_titles_hide(GtkCList* arg0)
{
  return gtk_clist_column_titles_hide(arg0);
}

void Pure_gtk_clist_column_title_active(GtkCList* arg0, int arg1)
{
  return gtk_clist_column_title_active(arg0, arg1);
}

void Pure_gtk_clist_column_title_passive(GtkCList* arg0, int arg1)
{
  return gtk_clist_column_title_passive(arg0, arg1);
}

void Pure_gtk_clist_column_titles_active(GtkCList* arg0)
{
  return gtk_clist_column_titles_active(arg0);
}

void Pure_gtk_clist_column_titles_passive(GtkCList* arg0)
{
  return gtk_clist_column_titles_passive(arg0);
}

void Pure_gtk_clist_set_column_title(GtkCList* arg0, int arg1, char const* arg2)
{
  return gtk_clist_set_column_title(arg0, arg1, arg2);
}

char* Pure_gtk_clist_get_column_title(GtkCList* arg0, int arg1)
{
  return gtk_clist_get_column_title(arg0, arg1);
}

void Pure_gtk_clist_set_column_widget(GtkCList* arg0, int arg1, GtkWidget* arg2)
{
  return gtk_clist_set_column_widget(arg0, arg1, arg2);
}

GtkWidget* Pure_gtk_clist_get_column_widget(GtkCList* arg0, int arg1)
{
  return gtk_clist_get_column_widget(arg0, arg1);
}

void Pure_gtk_clist_set_column_justification(GtkCList* arg0, int arg1, unsigned int arg2)
{
  return gtk_clist_set_column_justification(arg0, arg1, arg2);
}

void Pure_gtk_clist_set_column_visibility(GtkCList* arg0, int arg1, int arg2)
{
  return gtk_clist_set_column_visibility(arg0, arg1, arg2);
}

void Pure_gtk_clist_set_column_resizeable(GtkCList* arg0, int arg1, int arg2)
{
  return gtk_clist_set_column_resizeable(arg0, arg1, arg2);
}

void Pure_gtk_clist_set_column_auto_resize(GtkCList* arg0, int arg1, int arg2)
{
  return gtk_clist_set_column_auto_resize(arg0, arg1, arg2);
}

int Pure_gtk_clist_columns_autosize(GtkCList* arg0)
{
  return gtk_clist_columns_autosize(arg0);
}

int Pure_gtk_clist_optimal_column_width(GtkCList* arg0, int arg1)
{
  return gtk_clist_optimal_column_width(arg0, arg1);
}

void Pure_gtk_clist_set_column_width(GtkCList* arg0, int arg1, int arg2)
{
  return gtk_clist_set_column_width(arg0, arg1, arg2);
}

void Pure_gtk_clist_set_column_min_width(GtkCList* arg0, int arg1, int arg2)
{
  return gtk_clist_set_column_min_width(arg0, arg1, arg2);
}

void Pure_gtk_clist_set_column_max_width(GtkCList* arg0, int arg1, int arg2)
{
  return gtk_clist_set_column_max_width(arg0, arg1, arg2);
}

void Pure_gtk_clist_set_row_height(GtkCList* arg0, unsigned int arg1)
{
  return gtk_clist_set_row_height(arg0, arg1);
}

void Pure_gtk_clist_moveto(GtkCList* arg0, int arg1, int arg2, float arg3, float arg4)
{
  return gtk_clist_moveto(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_gtk_clist_row_is_visible(GtkCList* arg0, int arg1)
{
  return gtk_clist_row_is_visible(arg0, arg1);
}

unsigned int Pure_gtk_clist_get_cell_type(GtkCList* arg0, int arg1, int arg2)
{
  return gtk_clist_get_cell_type(arg0, arg1, arg2);
}

void Pure_gtk_clist_set_text(GtkCList* arg0, int arg1, int arg2, char const* arg3)
{
  return gtk_clist_set_text(arg0, arg1, arg2, arg3);
}

int Pure_gtk_clist_get_text(GtkCList* arg0, int arg1, int arg2, char** arg3)
{
  return gtk_clist_get_text(arg0, arg1, arg2, arg3);
}

void Pure_gtk_clist_set_pixmap(GtkCList* arg0, int arg1, int arg2, GdkPixmap* arg3, GdkBitmap* arg4)
{
  return gtk_clist_set_pixmap(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gtk_clist_get_pixmap(GtkCList* arg0, int arg1, int arg2, GdkPixmap** arg3, GdkBitmap** arg4)
{
  return gtk_clist_get_pixmap(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_clist_set_pixtext(GtkCList* arg0, int arg1, int arg2, char const* arg3, unsigned char arg4, GdkPixmap* arg5, GdkBitmap* arg6)
{
  return gtk_clist_set_pixtext(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

int Pure_gtk_clist_get_pixtext(GtkCList* arg0, int arg1, int arg2, char** arg3, unsigned char* arg4, GdkPixmap** arg5, GdkBitmap** arg6)
{
  return gtk_clist_get_pixtext(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_clist_set_foreground(GtkCList* arg0, int arg1, GdkColor const* arg2)
{
  return gtk_clist_set_foreground(arg0, arg1, arg2);
}

void Pure_gtk_clist_set_background(GtkCList* arg0, int arg1, GdkColor const* arg2)
{
  return gtk_clist_set_background(arg0, arg1, arg2);
}

void Pure_gtk_clist_set_cell_style(GtkCList* arg0, int arg1, int arg2, GtkStyle* arg3)
{
  return gtk_clist_set_cell_style(arg0, arg1, arg2, arg3);
}

GtkStyle* Pure_gtk_clist_get_cell_style(GtkCList* arg0, int arg1, int arg2)
{
  return gtk_clist_get_cell_style(arg0, arg1, arg2);
}

void Pure_gtk_clist_set_row_style(GtkCList* arg0, int arg1, GtkStyle* arg2)
{
  return gtk_clist_set_row_style(arg0, arg1, arg2);
}

GtkStyle* Pure_gtk_clist_get_row_style(GtkCList* arg0, int arg1)
{
  return gtk_clist_get_row_style(arg0, arg1);
}

void Pure_gtk_clist_set_shift(GtkCList* arg0, int arg1, int arg2, int arg3, int arg4)
{
  return gtk_clist_set_shift(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_clist_set_selectable(GtkCList* arg0, int arg1, int arg2)
{
  return gtk_clist_set_selectable(arg0, arg1, arg2);
}

int Pure_gtk_clist_get_selectable(GtkCList* arg0, int arg1)
{
  return gtk_clist_get_selectable(arg0, arg1);
}

int Pure_gtk_clist_prepend(GtkCList* arg0, char** arg1)
{
  return gtk_clist_prepend(arg0, arg1);
}

int Pure_gtk_clist_append(GtkCList* arg0, char** arg1)
{
  return gtk_clist_append(arg0, arg1);
}

int Pure_gtk_clist_insert(GtkCList* arg0, int arg1, char** arg2)
{
  return gtk_clist_insert(arg0, arg1, arg2);
}

void Pure_gtk_clist_remove(GtkCList* arg0, int arg1)
{
  return gtk_clist_remove(arg0, arg1);
}

void Pure_gtk_clist_set_row_data(GtkCList* arg0, int arg1, void* arg2)
{
  return gtk_clist_set_row_data(arg0, arg1, arg2);
}

void Pure_gtk_clist_set_row_data_full(GtkCList* arg0, int arg1, void* arg2, void* arg3)
{
  return gtk_clist_set_row_data_full(arg0, arg1, arg2, arg3);
}

void* Pure_gtk_clist_get_row_data(GtkCList* arg0, int arg1)
{
  return gtk_clist_get_row_data(arg0, arg1);
}

int Pure_gtk_clist_find_row_from_data(GtkCList* arg0, void* arg1)
{
  return gtk_clist_find_row_from_data(arg0, arg1);
}

void Pure_gtk_clist_select_row(GtkCList* arg0, int arg1, int arg2)
{
  return gtk_clist_select_row(arg0, arg1, arg2);
}

void Pure_gtk_clist_unselect_row(GtkCList* arg0, int arg1, int arg2)
{
  return gtk_clist_unselect_row(arg0, arg1, arg2);
}

void Pure_gtk_clist_undo_selection(GtkCList* arg0)
{
  return gtk_clist_undo_selection(arg0);
}

void Pure_gtk_clist_clear(GtkCList* arg0)
{
  return gtk_clist_clear(arg0);
}

int Pure_gtk_clist_get_selection_info(GtkCList* arg0, int arg1, int arg2, int* arg3, int* arg4)
{
  return gtk_clist_get_selection_info(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_clist_select_all(GtkCList* arg0)
{
  return gtk_clist_select_all(arg0);
}

void Pure_gtk_clist_unselect_all(GtkCList* arg0)
{
  return gtk_clist_unselect_all(arg0);
}

void Pure_gtk_clist_swap_rows(GtkCList* arg0, int arg1, int arg2)
{
  return gtk_clist_swap_rows(arg0, arg1, arg2);
}

void Pure_gtk_clist_row_move(GtkCList* arg0, int arg1, int arg2)
{
  return gtk_clist_row_move(arg0, arg1, arg2);
}

void Pure_gtk_clist_set_compare_func(GtkCList* arg0, void* arg1)
{
  return gtk_clist_set_compare_func(arg0, arg1);
}

void Pure_gtk_clist_set_sort_column(GtkCList* arg0, int arg1)
{
  return gtk_clist_set_sort_column(arg0, arg1);
}

void Pure_gtk_clist_set_sort_type(GtkCList* arg0, unsigned int arg1)
{
  return gtk_clist_set_sort_type(arg0, arg1);
}

void Pure_gtk_clist_sort(GtkCList* arg0)
{
  return gtk_clist_sort(arg0);
}

void Pure_gtk_clist_set_auto_sort(GtkCList* arg0, int arg1)
{
  return gtk_clist_set_auto_sort(arg0, arg1);
}

unsigned long Pure_gtk_combo_get_type()
{
  return gtk_combo_get_type();
}

GtkWidget* Pure_gtk_combo_new()
{
  return gtk_combo_new();
}

void Pure_gtk_combo_set_value_in_list(GtkCombo* arg0, int arg1, int arg2)
{
  return gtk_combo_set_value_in_list(arg0, arg1, arg2);
}

void Pure_gtk_combo_set_use_arrows(GtkCombo* arg0, int arg1)
{
  return gtk_combo_set_use_arrows(arg0, arg1);
}

void Pure_gtk_combo_set_use_arrows_always(GtkCombo* arg0, int arg1)
{
  return gtk_combo_set_use_arrows_always(arg0, arg1);
}

void Pure_gtk_combo_set_case_sensitive(GtkCombo* arg0, int arg1)
{
  return gtk_combo_set_case_sensitive(arg0, arg1);
}

void Pure_gtk_combo_set_item_string(GtkCombo* arg0, GtkItem* arg1, char const* arg2)
{
  return gtk_combo_set_item_string(arg0, arg1, arg2);
}

void Pure_gtk_combo_set_popdown_strings(GtkCombo* arg0, GList* arg1)
{
  return gtk_combo_set_popdown_strings(arg0, arg1);
}

void Pure_gtk_combo_disable_activate(GtkCombo* arg0)
{
  return gtk_combo_disable_activate(arg0);
}

unsigned long Pure_gtk_ctree_get_type()
{
  return gtk_ctree_get_type();
}

GtkWidget* Pure_gtk_ctree_new_with_titles(int arg0, int arg1, char** arg2)
{
  return gtk_ctree_new_with_titles(arg0, arg1, arg2);
}

GtkWidget* Pure_gtk_ctree_new(int arg0, int arg1)
{
  return gtk_ctree_new(arg0, arg1);
}

GtkCTreeNode* Pure_gtk_ctree_insert_node(GtkCTree* arg0, GtkCTreeNode* arg1, GtkCTreeNode* arg2, char** arg3, unsigned char arg4, GdkPixmap* arg5, GdkBitmap* arg6, GdkPixmap* arg7, GdkBitmap* arg8, int arg9, int arg10)
{
  return gtk_ctree_insert_node(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

void Pure_gtk_ctree_remove_node(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_remove_node(arg0, arg1);
}

GtkCTreeNode* Pure_gtk_ctree_insert_gnode(GtkCTree* arg0, GtkCTreeNode* arg1, GtkCTreeNode* arg2, GNode* arg3, void* arg4, void* arg5)
{
  return gtk_ctree_insert_gnode(arg0, arg1, arg2, arg3, arg4, arg5);
}

GNode* Pure_gtk_ctree_export_to_gnode(GtkCTree* arg0, GNode* arg1, GNode* arg2, GtkCTreeNode* arg3, void* arg4, void* arg5)
{
  return gtk_ctree_export_to_gnode(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_gtk_ctree_post_recursive(GtkCTree* arg0, GtkCTreeNode* arg1, void* arg2, void* arg3)
{
  return gtk_ctree_post_recursive(arg0, arg1, arg2, arg3);
}

void Pure_gtk_ctree_post_recursive_to_depth(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2, void* arg3, void* arg4)
{
  return gtk_ctree_post_recursive_to_depth(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_ctree_pre_recursive(GtkCTree* arg0, GtkCTreeNode* arg1, void* arg2, void* arg3)
{
  return gtk_ctree_pre_recursive(arg0, arg1, arg2, arg3);
}

void Pure_gtk_ctree_pre_recursive_to_depth(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2, void* arg3, void* arg4)
{
  return gtk_ctree_pre_recursive_to_depth(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gtk_ctree_is_viewable(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_is_viewable(arg0, arg1);
}

GtkCTreeNode* Pure_gtk_ctree_last(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_last(arg0, arg1);
}

GtkCTreeNode* Pure_gtk_ctree_find_node_ptr(GtkCTree* arg0, GtkCTreeRow* arg1)
{
  return gtk_ctree_find_node_ptr(arg0, arg1);
}

GtkCTreeNode* Pure_gtk_ctree_node_nth(GtkCTree* arg0, unsigned int arg1)
{
  return gtk_ctree_node_nth(arg0, arg1);
}

int Pure_gtk_ctree_find(GtkCTree* arg0, GtkCTreeNode* arg1, GtkCTreeNode* arg2)
{
  return gtk_ctree_find(arg0, arg1, arg2);
}

int Pure_gtk_ctree_is_ancestor(GtkCTree* arg0, GtkCTreeNode* arg1, GtkCTreeNode* arg2)
{
  return gtk_ctree_is_ancestor(arg0, arg1, arg2);
}

GtkCTreeNode* Pure_gtk_ctree_find_by_row_data(GtkCTree* arg0, GtkCTreeNode* arg1, void* arg2)
{
  return gtk_ctree_find_by_row_data(arg0, arg1, arg2);
}

GList* Pure_gtk_ctree_find_all_by_row_data(GtkCTree* arg0, GtkCTreeNode* arg1, void* arg2)
{
  return gtk_ctree_find_all_by_row_data(arg0, arg1, arg2);
}

GtkCTreeNode* Pure_gtk_ctree_find_by_row_data_custom(GtkCTree* arg0, GtkCTreeNode* arg1, void* arg2, void* arg3)
{
  return gtk_ctree_find_by_row_data_custom(arg0, arg1, arg2, arg3);
}

GList* Pure_gtk_ctree_find_all_by_row_data_custom(GtkCTree* arg0, GtkCTreeNode* arg1, void* arg2, void* arg3)
{
  return gtk_ctree_find_all_by_row_data_custom(arg0, arg1, arg2, arg3);
}

int Pure_gtk_ctree_is_hot_spot(GtkCTree* arg0, int arg1, int arg2)
{
  return gtk_ctree_is_hot_spot(arg0, arg1, arg2);
}

void Pure_gtk_ctree_move(GtkCTree* arg0, GtkCTreeNode* arg1, GtkCTreeNode* arg2, GtkCTreeNode* arg3)
{
  return gtk_ctree_move(arg0, arg1, arg2, arg3);
}

void Pure_gtk_ctree_expand(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_expand(arg0, arg1);
}

void Pure_gtk_ctree_expand_recursive(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_expand_recursive(arg0, arg1);
}

void Pure_gtk_ctree_expand_to_depth(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2)
{
  return gtk_ctree_expand_to_depth(arg0, arg1, arg2);
}

void Pure_gtk_ctree_collapse(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_collapse(arg0, arg1);
}

void Pure_gtk_ctree_collapse_recursive(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_collapse_recursive(arg0, arg1);
}

void Pure_gtk_ctree_collapse_to_depth(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2)
{
  return gtk_ctree_collapse_to_depth(arg0, arg1, arg2);
}

void Pure_gtk_ctree_toggle_expansion(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_toggle_expansion(arg0, arg1);
}

void Pure_gtk_ctree_toggle_expansion_recursive(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_toggle_expansion_recursive(arg0, arg1);
}

void Pure_gtk_ctree_select(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_select(arg0, arg1);
}

void Pure_gtk_ctree_select_recursive(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_select_recursive(arg0, arg1);
}

void Pure_gtk_ctree_unselect(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_unselect(arg0, arg1);
}

void Pure_gtk_ctree_unselect_recursive(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_unselect_recursive(arg0, arg1);
}

void Pure_gtk_ctree_real_select_recursive(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2)
{
  return gtk_ctree_real_select_recursive(arg0, arg1, arg2);
}

void Pure_gtk_ctree_node_set_text(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2, char const* arg3)
{
  return gtk_ctree_node_set_text(arg0, arg1, arg2, arg3);
}

void Pure_gtk_ctree_node_set_pixmap(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2, GdkPixmap* arg3, GdkBitmap* arg4)
{
  return gtk_ctree_node_set_pixmap(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_ctree_node_set_pixtext(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2, char const* arg3, unsigned char arg4, GdkPixmap* arg5, GdkBitmap* arg6)
{
  return gtk_ctree_node_set_pixtext(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_gtk_ctree_set_node_info(GtkCTree* arg0, GtkCTreeNode* arg1, char const* arg2, unsigned char arg3, GdkPixmap* arg4, GdkBitmap* arg5, GdkPixmap* arg6, GdkBitmap* arg7, int arg8, int arg9)
{
  return gtk_ctree_set_node_info(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_gtk_ctree_node_set_shift(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2, int arg3, int arg4)
{
  return gtk_ctree_node_set_shift(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_ctree_node_set_selectable(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2)
{
  return gtk_ctree_node_set_selectable(arg0, arg1, arg2);
}

int Pure_gtk_ctree_node_get_selectable(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_node_get_selectable(arg0, arg1);
}

unsigned int Pure_gtk_ctree_node_get_cell_type(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2)
{
  return gtk_ctree_node_get_cell_type(arg0, arg1, arg2);
}

int Pure_gtk_ctree_node_get_text(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2, char** arg3)
{
  return gtk_ctree_node_get_text(arg0, arg1, arg2, arg3);
}

int Pure_gtk_ctree_node_get_pixmap(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2, GdkPixmap** arg3, GdkBitmap** arg4)
{
  return gtk_ctree_node_get_pixmap(arg0, arg1, arg2, arg3, arg4);
}

int Pure_gtk_ctree_node_get_pixtext(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2, char** arg3, unsigned char* arg4, GdkPixmap** arg5, GdkBitmap** arg6)
{
  return gtk_ctree_node_get_pixtext(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

int Pure_gtk_ctree_get_node_info(GtkCTree* arg0, GtkCTreeNode* arg1, char** arg2, unsigned char* arg3, GdkPixmap** arg4, GdkBitmap** arg5, GdkPixmap** arg6, GdkBitmap** arg7, int* arg8, int* arg9)
{
  return gtk_ctree_get_node_info(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

void Pure_gtk_ctree_node_set_row_style(GtkCTree* arg0, GtkCTreeNode* arg1, GtkStyle* arg2)
{
  return gtk_ctree_node_set_row_style(arg0, arg1, arg2);
}

GtkStyle* Pure_gtk_ctree_node_get_row_style(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_node_get_row_style(arg0, arg1);
}

void Pure_gtk_ctree_node_set_cell_style(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2, GtkStyle* arg3)
{
  return gtk_ctree_node_set_cell_style(arg0, arg1, arg2, arg3);
}

GtkStyle* Pure_gtk_ctree_node_get_cell_style(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2)
{
  return gtk_ctree_node_get_cell_style(arg0, arg1, arg2);
}

void Pure_gtk_ctree_node_set_foreground(GtkCTree* arg0, GtkCTreeNode* arg1, GdkColor const* arg2)
{
  return gtk_ctree_node_set_foreground(arg0, arg1, arg2);
}

void Pure_gtk_ctree_node_set_background(GtkCTree* arg0, GtkCTreeNode* arg1, GdkColor const* arg2)
{
  return gtk_ctree_node_set_background(arg0, arg1, arg2);
}

void Pure_gtk_ctree_node_set_row_data(GtkCTree* arg0, GtkCTreeNode* arg1, void* arg2)
{
  return gtk_ctree_node_set_row_data(arg0, arg1, arg2);
}

void Pure_gtk_ctree_node_set_row_data_full(GtkCTree* arg0, GtkCTreeNode* arg1, void* arg2, void* arg3)
{
  return gtk_ctree_node_set_row_data_full(arg0, arg1, arg2, arg3);
}

void* Pure_gtk_ctree_node_get_row_data(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_node_get_row_data(arg0, arg1);
}

void Pure_gtk_ctree_node_moveto(GtkCTree* arg0, GtkCTreeNode* arg1, int arg2, float arg3, float arg4)
{
  return gtk_ctree_node_moveto(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_gtk_ctree_node_is_visible(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_node_is_visible(arg0, arg1);
}

void Pure_gtk_ctree_set_indent(GtkCTree* arg0, int arg1)
{
  return gtk_ctree_set_indent(arg0, arg1);
}

void Pure_gtk_ctree_set_spacing(GtkCTree* arg0, int arg1)
{
  return gtk_ctree_set_spacing(arg0, arg1);
}

void Pure_gtk_ctree_set_show_stub(GtkCTree* arg0, int arg1)
{
  return gtk_ctree_set_show_stub(arg0, arg1);
}

void Pure_gtk_ctree_set_line_style(GtkCTree* arg0, unsigned int arg1)
{
  return gtk_ctree_set_line_style(arg0, arg1);
}

void Pure_gtk_ctree_set_expander_style(GtkCTree* arg0, unsigned int arg1)
{
  return gtk_ctree_set_expander_style(arg0, arg1);
}

void Pure_gtk_ctree_set_drag_compare_func(GtkCTree* arg0, void* arg1)
{
  return gtk_ctree_set_drag_compare_func(arg0, arg1);
}

void Pure_gtk_ctree_sort_node(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_sort_node(arg0, arg1);
}

void Pure_gtk_ctree_sort_recursive(GtkCTree* arg0, GtkCTreeNode* arg1)
{
  return gtk_ctree_sort_recursive(arg0, arg1);
}

unsigned long Pure_gtk_ctree_node_get_type()
{
  return gtk_ctree_node_get_type();
}

unsigned long Pure_gtk_file_selection_get_type()
{
  return gtk_file_selection_get_type();
}

GtkWidget* Pure_gtk_file_selection_new(char const* arg0)
{
  return gtk_file_selection_new(arg0);
}

void Pure_gtk_file_selection_set_filename(GtkFileSelection* arg0, char const* arg1)
{
  return gtk_file_selection_set_filename(arg0, arg1);
}

char const* Pure_gtk_file_selection_get_filename(GtkFileSelection* arg0)
{
  return gtk_file_selection_get_filename(arg0);
}

void Pure_gtk_file_selection_complete(GtkFileSelection* arg0, char const* arg1)
{
  return gtk_file_selection_complete(arg0, arg1);
}

void Pure_gtk_file_selection_show_fileop_buttons(GtkFileSelection* arg0)
{
  return gtk_file_selection_show_fileop_buttons(arg0);
}

void Pure_gtk_file_selection_hide_fileop_buttons(GtkFileSelection* arg0)
{
  return gtk_file_selection_hide_fileop_buttons(arg0);
}

char** Pure_gtk_file_selection_get_selections(GtkFileSelection* arg0)
{
  return gtk_file_selection_get_selections(arg0);
}

void Pure_gtk_file_selection_set_select_multiple(GtkFileSelection* arg0, int arg1)
{
  return gtk_file_selection_set_select_multiple(arg0, arg1);
}

int Pure_gtk_file_selection_get_select_multiple(GtkFileSelection* arg0)
{
  return gtk_file_selection_get_select_multiple(arg0);
}

unsigned long Pure_gtk_item_factory_get_type()
{
  return gtk_item_factory_get_type();
}

GtkItemFactory* Pure_gtk_item_factory_new(unsigned long arg0, char const* arg1, GtkAccelGroup* arg2)
{
  return gtk_item_factory_new(arg0, arg1, arg2);
}

void Pure_gtk_item_factory_construct(GtkItemFactory* arg0, unsigned long arg1, char const* arg2, GtkAccelGroup* arg3)
{
  return gtk_item_factory_construct(arg0, arg1, arg2, arg3);
}

void Pure_gtk_item_factory_add_foreign(GtkWidget* arg0, char const* arg1, GtkAccelGroup* arg2, unsigned int arg3, unsigned int arg4)
{
  return gtk_item_factory_add_foreign(arg0, arg1, arg2, arg3, arg4);
}

GtkItemFactory* Pure_gtk_item_factory_from_widget(GtkWidget* arg0)
{
  return gtk_item_factory_from_widget(arg0);
}

char const* Pure_gtk_item_factory_path_from_widget(GtkWidget* arg0)
{
  return gtk_item_factory_path_from_widget(arg0);
}

GtkWidget* Pure_gtk_item_factory_get_item(GtkItemFactory* arg0, char const* arg1)
{
  return gtk_item_factory_get_item(arg0, arg1);
}

GtkWidget* Pure_gtk_item_factory_get_widget(GtkItemFactory* arg0, char const* arg1)
{
  return gtk_item_factory_get_widget(arg0, arg1);
}

GtkWidget* Pure_gtk_item_factory_get_widget_by_action(GtkItemFactory* arg0, unsigned int arg1)
{
  return gtk_item_factory_get_widget_by_action(arg0, arg1);
}

GtkWidget* Pure_gtk_item_factory_get_item_by_action(GtkItemFactory* arg0, unsigned int arg1)
{
  return gtk_item_factory_get_item_by_action(arg0, arg1);
}

void Pure_gtk_item_factory_create_item(GtkItemFactory* arg0, GtkItemFactoryEntry* arg1, void* arg2, unsigned int arg3)
{
  return gtk_item_factory_create_item(arg0, arg1, arg2, arg3);
}

void Pure_gtk_item_factory_create_items(GtkItemFactory* arg0, unsigned int arg1, GtkItemFactoryEntry* arg2, void* arg3)
{
  return gtk_item_factory_create_items(arg0, arg1, arg2, arg3);
}

void Pure_gtk_item_factory_delete_item(GtkItemFactory* arg0, char const* arg1)
{
  return gtk_item_factory_delete_item(arg0, arg1);
}

void Pure_gtk_item_factory_delete_entry(GtkItemFactory* arg0, GtkItemFactoryEntry* arg1)
{
  return gtk_item_factory_delete_entry(arg0, arg1);
}

void Pure_gtk_item_factory_delete_entries(GtkItemFactory* arg0, unsigned int arg1, GtkItemFactoryEntry* arg2)
{
  return gtk_item_factory_delete_entries(arg0, arg1, arg2);
}

void Pure_gtk_item_factory_popup(GtkItemFactory* arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4)
{
  return gtk_item_factory_popup(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_item_factory_popup_with_data(GtkItemFactory* arg0, void* arg1, void* arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6)
{
  return gtk_item_factory_popup_with_data(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void* Pure_gtk_item_factory_popup_data(GtkItemFactory* arg0)
{
  return gtk_item_factory_popup_data(arg0);
}

void* Pure_gtk_item_factory_popup_data_from_widget(GtkWidget* arg0)
{
  return gtk_item_factory_popup_data_from_widget(arg0);
}

void Pure_gtk_item_factory_set_translate_func(GtkItemFactory* arg0, void* arg1, void* arg2, void* arg3)
{
  return gtk_item_factory_set_translate_func(arg0, arg1, arg2, arg3);
}

void Pure_gtk_item_factory_create_items_ac(GtkItemFactory* arg0, unsigned int arg1, GtkItemFactoryEntry* arg2, void* arg3, unsigned int arg4)
{
  return gtk_item_factory_create_items_ac(arg0, arg1, arg2, arg3, arg4);
}

GtkItemFactory* Pure_gtk_item_factory_from_path(char const* arg0)
{
  return gtk_item_factory_from_path(arg0);
}

void Pure_gtk_item_factory_create_menu_entries(unsigned int arg0, GtkMenuEntry* arg1)
{
  return gtk_item_factory_create_menu_entries(arg0, arg1);
}

void Pure_gtk_item_factories_path_delete(char const* arg0, char const* arg1)
{
  return gtk_item_factories_path_delete(arg0, arg1);
}

unsigned long Pure_gtk_list_get_type()
{
  return gtk_list_get_type();
}

GtkWidget* Pure_gtk_list_new()
{
  return gtk_list_new();
}

void Pure_gtk_list_insert_items(GtkList* arg0, GList* arg1, int arg2)
{
  return gtk_list_insert_items(arg0, arg1, arg2);
}

void Pure_gtk_list_append_items(GtkList* arg0, GList* arg1)
{
  return gtk_list_append_items(arg0, arg1);
}

void Pure_gtk_list_prepend_items(GtkList* arg0, GList* arg1)
{
  return gtk_list_prepend_items(arg0, arg1);
}

void Pure_gtk_list_remove_items(GtkList* arg0, GList* arg1)
{
  return gtk_list_remove_items(arg0, arg1);
}

void Pure_gtk_list_remove_items_no_unref(GtkList* arg0, GList* arg1)
{
  return gtk_list_remove_items_no_unref(arg0, arg1);
}

void Pure_gtk_list_clear_items(GtkList* arg0, int arg1, int arg2)
{
  return gtk_list_clear_items(arg0, arg1, arg2);
}

void Pure_gtk_list_select_item(GtkList* arg0, int arg1)
{
  return gtk_list_select_item(arg0, arg1);
}

void Pure_gtk_list_unselect_item(GtkList* arg0, int arg1)
{
  return gtk_list_unselect_item(arg0, arg1);
}

void Pure_gtk_list_select_child(GtkList* arg0, GtkWidget* arg1)
{
  return gtk_list_select_child(arg0, arg1);
}

void Pure_gtk_list_unselect_child(GtkList* arg0, GtkWidget* arg1)
{
  return gtk_list_unselect_child(arg0, arg1);
}

int Pure_gtk_list_child_position(GtkList* arg0, GtkWidget* arg1)
{
  return gtk_list_child_position(arg0, arg1);
}

void Pure_gtk_list_set_selection_mode(GtkList* arg0, unsigned int arg1)
{
  return gtk_list_set_selection_mode(arg0, arg1);
}

void Pure_gtk_list_extend_selection(GtkList* arg0, unsigned int arg1, float arg2, int arg3)
{
  return gtk_list_extend_selection(arg0, arg1, arg2, arg3);
}

void Pure_gtk_list_start_selection(GtkList* arg0)
{
  return gtk_list_start_selection(arg0);
}

void Pure_gtk_list_end_selection(GtkList* arg0)
{
  return gtk_list_end_selection(arg0);
}

void Pure_gtk_list_select_all(GtkList* arg0)
{
  return gtk_list_select_all(arg0);
}

void Pure_gtk_list_unselect_all(GtkList* arg0)
{
  return gtk_list_unselect_all(arg0);
}

void Pure_gtk_list_scroll_horizontal(GtkList* arg0, unsigned int arg1, float arg2)
{
  return gtk_list_scroll_horizontal(arg0, arg1, arg2);
}

void Pure_gtk_list_scroll_vertical(GtkList* arg0, unsigned int arg1, float arg2)
{
  return gtk_list_scroll_vertical(arg0, arg1, arg2);
}

void Pure_gtk_list_toggle_add_mode(GtkList* arg0)
{
  return gtk_list_toggle_add_mode(arg0);
}

void Pure_gtk_list_toggle_focus_row(GtkList* arg0)
{
  return gtk_list_toggle_focus_row(arg0);
}

void Pure_gtk_list_toggle_row(GtkList* arg0, GtkWidget* arg1)
{
  return gtk_list_toggle_row(arg0, arg1);
}

void Pure_gtk_list_undo_selection(GtkList* arg0)
{
  return gtk_list_undo_selection(arg0);
}

void Pure_gtk_list_end_drag_selection(GtkList* arg0)
{
  return gtk_list_end_drag_selection(arg0);
}

unsigned long Pure_gtk_list_item_get_type()
{
  return gtk_list_item_get_type();
}

GtkWidget* Pure_gtk_list_item_new()
{
  return gtk_list_item_new();
}

GtkWidget* Pure_gtk_list_item_new_with_label(char const* arg0)
{
  return gtk_list_item_new_with_label(arg0);
}

void Pure_gtk_list_item_select(GtkListItem* arg0)
{
  return gtk_list_item_select(arg0);
}

void Pure_gtk_list_item_deselect(GtkListItem* arg0)
{
  return gtk_list_item_deselect(arg0);
}

unsigned long Pure_gtk_old_editable_get_type()
{
  return gtk_old_editable_get_type();
}

void Pure_gtk_old_editable_claim_selection(GtkOldEditable* arg0, int arg1, unsigned int arg2)
{
  return gtk_old_editable_claim_selection(arg0, arg1, arg2);
}

void Pure_gtk_old_editable_changed(GtkOldEditable* arg0)
{
  return gtk_old_editable_changed(arg0);
}

unsigned long Pure_gtk_option_menu_get_type()
{
  return gtk_option_menu_get_type();
}

GtkWidget* Pure_gtk_option_menu_new()
{
  return gtk_option_menu_new();
}

GtkWidget* Pure_gtk_option_menu_get_menu(GtkOptionMenu* arg0)
{
  return gtk_option_menu_get_menu(arg0);
}

void Pure_gtk_option_menu_set_menu(GtkOptionMenu* arg0, GtkWidget* arg1)
{
  return gtk_option_menu_set_menu(arg0, arg1);
}

void Pure_gtk_option_menu_remove_menu(GtkOptionMenu* arg0)
{
  return gtk_option_menu_remove_menu(arg0);
}

int Pure_gtk_option_menu_get_history(GtkOptionMenu* arg0)
{
  return gtk_option_menu_get_history(arg0);
}

void Pure_gtk_option_menu_set_history(GtkOptionMenu* arg0, unsigned int arg1)
{
  return gtk_option_menu_set_history(arg0, arg1);
}

unsigned long Pure_gtk_preview_get_type()
{
  return gtk_preview_get_type();
}

void Pure_gtk_preview_uninit()
{
  return gtk_preview_uninit();
}

GtkWidget* Pure_gtk_preview_new(unsigned int arg0)
{
  return gtk_preview_new(arg0);
}

void Pure_gtk_preview_size(GtkPreview* arg0, int arg1, int arg2)
{
  return gtk_preview_size(arg0, arg1, arg2);
}

void Pure_gtk_preview_put(GtkPreview* arg0, GdkWindow* arg1, GdkGC* arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8)
{
  return gtk_preview_put(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_gtk_preview_draw_row(GtkPreview* arg0, unsigned char* arg1, int arg2, int arg3, int arg4)
{
  return gtk_preview_draw_row(arg0, arg1, arg2, arg3, arg4);
}

void Pure_gtk_preview_set_expand(GtkPreview* arg0, int arg1)
{
  return gtk_preview_set_expand(arg0, arg1);
}

void Pure_gtk_preview_set_gamma(double arg0)
{
  return gtk_preview_set_gamma(arg0);
}

void Pure_gtk_preview_set_color_cube(unsigned int arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3)
{
  return gtk_preview_set_color_cube(arg0, arg1, arg2, arg3);
}

void Pure_gtk_preview_set_install_cmap(int arg0)
{
  return gtk_preview_set_install_cmap(arg0);
}

void Pure_gtk_preview_set_reserved(int arg0)
{
  return gtk_preview_set_reserved(arg0);
}

void Pure_gtk_preview_set_dither(GtkPreview* arg0, unsigned int arg1)
{
  return gtk_preview_set_dither(arg0, arg1);
}

GdkVisual* Pure_gtk_preview_get_visual()
{
  return gtk_preview_get_visual();
}

GdkColormap* Pure_gtk_preview_get_cmap()
{
  return gtk_preview_get_cmap();
}

GtkPreviewInfo* Pure_gtk_preview_get_info()
{
  return gtk_preview_get_info();
}

void Pure_gtk_preview_reset()
{
  return gtk_preview_reset();
}

unsigned long Pure_gtk_tips_query_get_type()
{
  return gtk_tips_query_get_type();
}

GtkWidget* Pure_gtk_tips_query_new()
{
  return gtk_tips_query_new();
}

void Pure_gtk_tips_query_start_query(GtkTipsQuery* arg0)
{
  return gtk_tips_query_start_query(arg0);
}

void Pure_gtk_tips_query_stop_query(GtkTipsQuery* arg0)
{
  return gtk_tips_query_stop_query(arg0);
}

void Pure_gtk_tips_query_set_caller(GtkTipsQuery* arg0, GtkWidget* arg1)
{
  return gtk_tips_query_set_caller(arg0, arg1);
}

void Pure_gtk_tips_query_set_labels(GtkTipsQuery* arg0, char const* arg1, char const* arg2)
{
  return gtk_tips_query_set_labels(arg0, arg1, arg2);
}
