#include <glib.h>

GArray* Pure_g_array_new(int arg0, int arg1, unsigned int arg2)
{
  return g_array_new(arg0, arg1, arg2);
}

GArray* Pure_g_array_sized_new(int arg0, int arg1, unsigned int arg2, unsigned int arg3)
{
  return g_array_sized_new(arg0, arg1, arg2, arg3);
}

char* Pure_g_array_free(GArray* arg0, int arg1)
{
  return g_array_free(arg0, arg1);
}

GArray* Pure_g_array_append_vals(GArray* arg0, void const* arg1, unsigned int arg2)
{
  return g_array_append_vals(arg0, arg1, arg2);
}

GArray* Pure_g_array_prepend_vals(GArray* arg0, void const* arg1, unsigned int arg2)
{
  return g_array_prepend_vals(arg0, arg1, arg2);
}

GArray* Pure_g_array_insert_vals(GArray* arg0, unsigned int arg1, void const* arg2, unsigned int arg3)
{
  return g_array_insert_vals(arg0, arg1, arg2, arg3);
}

GArray* Pure_g_array_set_size(GArray* arg0, unsigned int arg1)
{
  return g_array_set_size(arg0, arg1);
}

GArray* Pure_g_array_remove_index(GArray* arg0, unsigned int arg1)
{
  return g_array_remove_index(arg0, arg1);
}

GArray* Pure_g_array_remove_index_fast(GArray* arg0, unsigned int arg1)
{
  return g_array_remove_index_fast(arg0, arg1);
}

GArray* Pure_g_array_remove_range(GArray* arg0, unsigned int arg1, unsigned int arg2)
{
  return g_array_remove_range(arg0, arg1, arg2);
}

void Pure_g_array_sort(GArray* arg0, void* arg1)
{
  return g_array_sort(arg0, arg1);
}

void Pure_g_array_sort_with_data(GArray* arg0, void* arg1, void* arg2)
{
  return g_array_sort_with_data(arg0, arg1, arg2);
}

GPtrArray* Pure_g_ptr_array_new()
{
  return g_ptr_array_new();
}

GPtrArray* Pure_g_ptr_array_sized_new(unsigned int arg0)
{
  return g_ptr_array_sized_new(arg0);
}

void** Pure_g_ptr_array_free(GPtrArray* arg0, int arg1)
{
  return g_ptr_array_free(arg0, arg1);
}

void Pure_g_ptr_array_set_size(GPtrArray* arg0, int arg1)
{
  return g_ptr_array_set_size(arg0, arg1);
}

void* Pure_g_ptr_array_remove_index(GPtrArray* arg0, unsigned int arg1)
{
  return g_ptr_array_remove_index(arg0, arg1);
}

void* Pure_g_ptr_array_remove_index_fast(GPtrArray* arg0, unsigned int arg1)
{
  return g_ptr_array_remove_index_fast(arg0, arg1);
}

int Pure_g_ptr_array_remove(GPtrArray* arg0, void* arg1)
{
  return g_ptr_array_remove(arg0, arg1);
}

int Pure_g_ptr_array_remove_fast(GPtrArray* arg0, void* arg1)
{
  return g_ptr_array_remove_fast(arg0, arg1);
}

void Pure_g_ptr_array_remove_range(GPtrArray* arg0, unsigned int arg1, unsigned int arg2)
{
  return g_ptr_array_remove_range(arg0, arg1, arg2);
}

void Pure_g_ptr_array_add(GPtrArray* arg0, void* arg1)
{
  return g_ptr_array_add(arg0, arg1);
}

void Pure_g_ptr_array_sort(GPtrArray* arg0, void* arg1)
{
  return g_ptr_array_sort(arg0, arg1);
}

void Pure_g_ptr_array_sort_with_data(GPtrArray* arg0, void* arg1, void* arg2)
{
  return g_ptr_array_sort_with_data(arg0, arg1, arg2);
}

void Pure_g_ptr_array_foreach(GPtrArray* arg0, void* arg1, void* arg2)
{
  return g_ptr_array_foreach(arg0, arg1, arg2);
}

GByteArray* Pure_g_byte_array_new()
{
  return g_byte_array_new();
}

GByteArray* Pure_g_byte_array_sized_new(unsigned int arg0)
{
  return g_byte_array_sized_new(arg0);
}

unsigned char* Pure_g_byte_array_free(GByteArray* arg0, int arg1)
{
  return g_byte_array_free(arg0, arg1);
}

GByteArray* Pure_g_byte_array_append(GByteArray* arg0, unsigned char const* arg1, unsigned int arg2)
{
  return g_byte_array_append(arg0, arg1, arg2);
}

GByteArray* Pure_g_byte_array_prepend(GByteArray* arg0, unsigned char const* arg1, unsigned int arg2)
{
  return g_byte_array_prepend(arg0, arg1, arg2);
}

GByteArray* Pure_g_byte_array_set_size(GByteArray* arg0, unsigned int arg1)
{
  return g_byte_array_set_size(arg0, arg1);
}

GByteArray* Pure_g_byte_array_remove_index(GByteArray* arg0, unsigned int arg1)
{
  return g_byte_array_remove_index(arg0, arg1);
}

GByteArray* Pure_g_byte_array_remove_index_fast(GByteArray* arg0, unsigned int arg1)
{
  return g_byte_array_remove_index_fast(arg0, arg1);
}

GByteArray* Pure_g_byte_array_remove_range(GByteArray* arg0, unsigned int arg1, unsigned int arg2)
{
  return g_byte_array_remove_range(arg0, arg1, arg2);
}

void Pure_g_byte_array_sort(GByteArray* arg0, void* arg1)
{
  return g_byte_array_sort(arg0, arg1);
}

void Pure_g_byte_array_sort_with_data(GByteArray* arg0, void* arg1, void* arg2)
{
  return g_byte_array_sort_with_data(arg0, arg1, arg2);
}

unsigned int Pure_g_quark_try_string(char const* arg0)
{
  return g_quark_try_string(arg0);
}

unsigned int Pure_g_quark_from_static_string(char const* arg0)
{
  return g_quark_from_static_string(arg0);
}

unsigned int Pure_g_quark_from_string(char const* arg0)
{
  return g_quark_from_string(arg0);
}

char const* Pure_g_quark_to_string(unsigned int arg0)
{
  return g_quark_to_string(arg0);
}

char const* Pure_g_intern_string(char const* arg0)
{
  return g_intern_string(arg0);
}

char const* Pure_g_intern_static_string(char const* arg0)
{
  return g_intern_static_string(arg0);
}

GError* Pure_g_error_new(unsigned int arg0, int arg1, char const* arg2)
{
  return g_error_new(arg0, arg1, arg2);
}

GError* Pure_g_error_new_literal(unsigned int arg0, int arg1, char const* arg2)
{
  return g_error_new_literal(arg0, arg1, arg2);
}

void Pure_g_error_free(GError* arg0)
{
  return g_error_free(arg0);
}

GError* Pure_g_error_copy(GError const* arg0)
{
  return g_error_copy(arg0);
}

int Pure_g_error_matches(GError const* arg0, unsigned int arg1, int arg2)
{
  return g_error_matches(arg0, arg1, arg2);
}

void Pure_g_set_error(GError** arg0, unsigned int arg1, int arg2, char const* arg3)
{
  return g_set_error(arg0, arg1, arg2, arg3);
}

void Pure_g_set_error_literal(GError** arg0, unsigned int arg1, int arg2, char const* arg3)
{
  return g_set_error_literal(arg0, arg1, arg2, arg3);
}

void Pure_g_propagate_error(GError** arg0, GError* arg1)
{
  return g_propagate_error(arg0, arg1);
}

void Pure_g_clear_error(GError** arg0)
{
  return g_clear_error(arg0);
}

void Pure_g_prefix_error(GError** arg0, char const* arg1)
{
  return g_prefix_error(arg0, arg1);
}

void Pure_g_propagate_prefixed_error(GError** arg0, GError* arg1, char const* arg2)
{
  return g_propagate_prefixed_error(arg0, arg1, arg2);
}

char const* Pure_g_get_user_name()
{
  return g_get_user_name();
}

char const* Pure_g_get_real_name()
{
  return g_get_real_name();
}

char const* Pure_g_get_home_dir()
{
  return g_get_home_dir();
}

char const* Pure_g_get_tmp_dir()
{
  return g_get_tmp_dir();
}

char const* Pure_g_get_host_name()
{
  return g_get_host_name();
}

char* Pure_g_get_prgname()
{
  return g_get_prgname();
}

void Pure_g_set_prgname(char const* arg0)
{
  return g_set_prgname(arg0);
}

char const* Pure_g_get_application_name()
{
  return g_get_application_name();
}

void Pure_g_set_application_name(char const* arg0)
{
  return g_set_application_name(arg0);
}

char const* Pure_g_get_user_data_dir()
{
  return g_get_user_data_dir();
}

char const* Pure_g_get_user_config_dir()
{
  return g_get_user_config_dir();
}

char const* Pure_g_get_user_cache_dir()
{
  return g_get_user_cache_dir();
}

char const* const* Pure_g_get_system_data_dirs()
{
  return g_get_system_data_dirs();
}

char const* const* Pure_g_get_system_config_dirs()
{
  return g_get_system_config_dirs();
}

char const* const* Pure_g_get_language_names()
{
  return g_get_language_names();
}

char const* Pure_g_get_user_special_dir(unsigned int arg0)
{
  return g_get_user_special_dir(arg0);
}

unsigned int Pure_g_parse_debug_string(char const* arg0, GDebugKey const* arg1, unsigned int arg2)
{
  return g_parse_debug_string(arg0, arg1, arg2);
}

int Pure_g_snprintf(char* arg0, unsigned long arg1, char* arg2)
{
  return g_snprintf(arg0, arg1, arg2);
}

int Pure_g_vsnprintf(char* arg0, unsigned long arg1, char* arg2, void* arg3)
{
  return g_vsnprintf(arg0, arg1, arg2, arg3);
}

int Pure_g_path_is_absolute(char const* arg0)
{
  return g_path_is_absolute(arg0);
}

char const* Pure_g_path_skip_root(char const* arg0)
{
  return g_path_skip_root(arg0);
}

char const* Pure_g_basename(char const* arg0)
{
  return g_basename(arg0);
}

char* Pure_g_get_current_dir()
{
  return g_get_current_dir();
}

char* Pure_g_path_get_basename(char const* arg0)
{
  return g_path_get_basename(arg0);
}

char* Pure_g_path_get_dirname(char const* arg0)
{
  return g_path_get_dirname(arg0);
}

void Pure_g_nullify_pointer(void** arg0)
{
  return g_nullify_pointer(arg0);
}

char const* Pure_g_getenv(char const* arg0)
{
  return g_getenv(arg0);
}

int Pure_g_setenv(char const* arg0, char const* arg1, int arg2)
{
  return g_setenv(arg0, arg1, arg2);
}

void Pure_g_unsetenv(char const* arg0)
{
  return g_unsetenv(arg0);
}

char** Pure_g_listenv()
{
  return g_listenv();
}

void Pure_g_atexit(void* arg0)
{
  return g_atexit(arg0);
}

char* Pure_g_find_program_in_path(char const* arg0)
{
  return g_find_program_in_path(arg0);
}

int Pure_g_bit_nth_lsf(unsigned long arg0, int arg1)
{
  return g_bit_nth_lsf(arg0, arg1);
}

int Pure_g_bit_nth_msf(unsigned long arg0, int arg1)
{
  return g_bit_nth_msf(arg0, arg1);
}

unsigned int Pure_g_bit_storage(unsigned long arg0)
{
  return g_bit_storage(arg0);
}

void Pure_g_trash_stack_push(GTrashStack** arg0, void* arg1)
{
  return g_trash_stack_push(arg0, arg1);
}

void* Pure_g_trash_stack_pop(GTrashStack** arg0)
{
  return g_trash_stack_pop(arg0);
}

void* Pure_g_trash_stack_peek(GTrashStack** arg0)
{
  return g_trash_stack_peek(arg0);
}

unsigned int Pure_g_trash_stack_height(GTrashStack** arg0)
{
  return g_trash_stack_height(arg0);
}

char const* Pure_glib_check_version(unsigned int arg0, unsigned int arg1, unsigned int arg2)
{
  return glib_check_version(arg0, arg1, arg2);
}

int Pure_g_atomic_int_exchange_and_add(int* arg0, int arg1)
{
  return g_atomic_int_exchange_and_add(arg0, arg1);
}

void Pure_g_atomic_int_add(int* arg0, int arg1)
{
  return g_atomic_int_add(arg0, arg1);
}

int Pure_g_atomic_int_compare_and_exchange(int* arg0, int arg1, int arg2)
{
  return g_atomic_int_compare_and_exchange(arg0, arg1, arg2);
}

int Pure_g_atomic_pointer_compare_and_exchange(void** arg0, void* arg1, void* arg2)
{
  return g_atomic_pointer_compare_and_exchange(arg0, arg1, arg2);
}

int Pure_g_atomic_int_get(int* arg0)
{
  return g_atomic_int_get(arg0);
}

void Pure_g_atomic_int_set(int* arg0, int arg1)
{
  return g_atomic_int_set(arg0, arg1);
}

void* Pure_g_atomic_pointer_get(void** arg0)
{
  return g_atomic_pointer_get(arg0);
}

void Pure_g_atomic_pointer_set(void** arg0, void* arg1)
{
  return g_atomic_pointer_set(arg0, arg1);
}

unsigned int Pure_g_thread_error_quark()
{
  return g_thread_error_quark();
}

GMutex* Pure_g_static_mutex_get_mutex_impl(GMutex** arg0)
{
  return g_static_mutex_get_mutex_impl(arg0);
}

GThread* Pure_g_thread_create_full(void* arg0, void* arg1, unsigned long arg2, int arg3, int arg4, unsigned int arg5, GError** arg6)
{
  return g_thread_create_full(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GThread* Pure_g_thread_self()
{
  return g_thread_self();
}

void Pure_g_thread_exit(void* arg0)
{
  return g_thread_exit(arg0);
}

void* Pure_g_thread_join(GThread* arg0)
{
  return g_thread_join(arg0);
}

void Pure_g_thread_set_priority(GThread* arg0, unsigned int arg1)
{
  return g_thread_set_priority(arg0, arg1);
}

void Pure_g_static_mutex_init(GStaticMutex* arg0)
{
  return g_static_mutex_init(arg0);
}

void Pure_g_static_mutex_free(GStaticMutex* arg0)
{
  return g_static_mutex_free(arg0);
}

void Pure_g_static_private_init(GStaticPrivate* arg0)
{
  return g_static_private_init(arg0);
}

void* Pure_g_static_private_get(GStaticPrivate* arg0)
{
  return g_static_private_get(arg0);
}

void Pure_g_static_private_set(GStaticPrivate* arg0, void* arg1, void* arg2)
{
  return g_static_private_set(arg0, arg1, arg2);
}

void Pure_g_static_private_free(GStaticPrivate* arg0)
{
  return g_static_private_free(arg0);
}

void Pure_g_static_rec_mutex_init(GStaticRecMutex* arg0)
{
  return g_static_rec_mutex_init(arg0);
}

void Pure_g_static_rec_mutex_lock(GStaticRecMutex* arg0)
{
  return g_static_rec_mutex_lock(arg0);
}

int Pure_g_static_rec_mutex_trylock(GStaticRecMutex* arg0)
{
  return g_static_rec_mutex_trylock(arg0);
}

void Pure_g_static_rec_mutex_unlock(GStaticRecMutex* arg0)
{
  return g_static_rec_mutex_unlock(arg0);
}

void Pure_g_static_rec_mutex_lock_full(GStaticRecMutex* arg0, unsigned int arg1)
{
  return g_static_rec_mutex_lock_full(arg0, arg1);
}

unsigned int Pure_g_static_rec_mutex_unlock_full(GStaticRecMutex* arg0)
{
  return g_static_rec_mutex_unlock_full(arg0);
}

void Pure_g_static_rec_mutex_free(GStaticRecMutex* arg0)
{
  return g_static_rec_mutex_free(arg0);
}

void Pure_g_static_rw_lock_init(GStaticRWLock* arg0)
{
  return g_static_rw_lock_init(arg0);
}

void Pure_g_static_rw_lock_reader_lock(GStaticRWLock* arg0)
{
  return g_static_rw_lock_reader_lock(arg0);
}

int Pure_g_static_rw_lock_reader_trylock(GStaticRWLock* arg0)
{
  return g_static_rw_lock_reader_trylock(arg0);
}

void Pure_g_static_rw_lock_reader_unlock(GStaticRWLock* arg0)
{
  return g_static_rw_lock_reader_unlock(arg0);
}

void Pure_g_static_rw_lock_writer_lock(GStaticRWLock* arg0)
{
  return g_static_rw_lock_writer_lock(arg0);
}

int Pure_g_static_rw_lock_writer_trylock(GStaticRWLock* arg0)
{
  return g_static_rw_lock_writer_trylock(arg0);
}

void Pure_g_static_rw_lock_writer_unlock(GStaticRWLock* arg0)
{
  return g_static_rw_lock_writer_unlock(arg0);
}

void Pure_g_static_rw_lock_free(GStaticRWLock* arg0)
{
  return g_static_rw_lock_free(arg0);
}

void Pure_g_thread_foreach(void* arg0, void* arg1)
{
  return g_thread_foreach(arg0, arg1);
}

void* Pure_g_once_impl(GOnce* arg0, void* arg1, void* arg2)
{
  return g_once_impl(arg0, arg1, arg2);
}

int Pure_g_once_init_enter(unsigned long* arg0)
{
  return g_once_init_enter(arg0);
}

int Pure_g_once_init_enter_impl(unsigned long* arg0)
{
  return g_once_init_enter_impl(arg0);
}

void Pure_g_once_init_leave(unsigned long* arg0, unsigned long arg1)
{
  return g_once_init_leave(arg0, arg1);
}

GAsyncQueue* Pure_g_async_queue_new()
{
  return g_async_queue_new();
}

GAsyncQueue* Pure_g_async_queue_new_full(void* arg0)
{
  return g_async_queue_new_full(arg0);
}

void Pure_g_async_queue_lock(GAsyncQueue* arg0)
{
  return g_async_queue_lock(arg0);
}

void Pure_g_async_queue_unlock(GAsyncQueue* arg0)
{
  return g_async_queue_unlock(arg0);
}

GAsyncQueue* Pure_g_async_queue_ref(GAsyncQueue* arg0)
{
  return g_async_queue_ref(arg0);
}

void Pure_g_async_queue_unref(GAsyncQueue* arg0)
{
  return g_async_queue_unref(arg0);
}

void Pure_g_async_queue_ref_unlocked(GAsyncQueue* arg0)
{
  return g_async_queue_ref_unlocked(arg0);
}

void Pure_g_async_queue_unref_and_unlock(GAsyncQueue* arg0)
{
  return g_async_queue_unref_and_unlock(arg0);
}

void Pure_g_async_queue_push(GAsyncQueue* arg0, void* arg1)
{
  return g_async_queue_push(arg0, arg1);
}

void Pure_g_async_queue_push_unlocked(GAsyncQueue* arg0, void* arg1)
{
  return g_async_queue_push_unlocked(arg0, arg1);
}

void Pure_g_async_queue_push_sorted(GAsyncQueue* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_async_queue_push_sorted(arg0, arg1, arg2, arg3);
}

void Pure_g_async_queue_push_sorted_unlocked(GAsyncQueue* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_async_queue_push_sorted_unlocked(arg0, arg1, arg2, arg3);
}

void* Pure_g_async_queue_pop(GAsyncQueue* arg0)
{
  return g_async_queue_pop(arg0);
}

void* Pure_g_async_queue_pop_unlocked(GAsyncQueue* arg0)
{
  return g_async_queue_pop_unlocked(arg0);
}

void* Pure_g_async_queue_try_pop(GAsyncQueue* arg0)
{
  return g_async_queue_try_pop(arg0);
}

void* Pure_g_async_queue_try_pop_unlocked(GAsyncQueue* arg0)
{
  return g_async_queue_try_pop_unlocked(arg0);
}

void* Pure_g_async_queue_timed_pop(GAsyncQueue* arg0, GTimeVal* arg1)
{
  return g_async_queue_timed_pop(arg0, arg1);
}

void* Pure_g_async_queue_timed_pop_unlocked(GAsyncQueue* arg0, GTimeVal* arg1)
{
  return g_async_queue_timed_pop_unlocked(arg0, arg1);
}

int Pure_g_async_queue_length(GAsyncQueue* arg0)
{
  return g_async_queue_length(arg0);
}

int Pure_g_async_queue_length_unlocked(GAsyncQueue* arg0)
{
  return g_async_queue_length_unlocked(arg0);
}

void Pure_g_async_queue_sort(GAsyncQueue* arg0, void* arg1, void* arg2)
{
  return g_async_queue_sort(arg0, arg1, arg2);
}

void Pure_g_async_queue_sort_unlocked(GAsyncQueue* arg0, void* arg1, void* arg2)
{
  return g_async_queue_sort_unlocked(arg0, arg1, arg2);
}

void Pure_g_on_error_query(char const* arg0)
{
  return g_on_error_query(arg0);
}

void Pure_g_on_error_stack_trace(char const* arg0)
{
  return g_on_error_stack_trace(arg0);
}

unsigned long Pure_g_base64_encode_step(unsigned char const* arg0, unsigned long arg1, int arg2, char* arg3, int* arg4, int* arg5)
{
  return g_base64_encode_step(arg0, arg1, arg2, arg3, arg4, arg5);
}

unsigned long Pure_g_base64_encode_close(int arg0, char* arg1, int* arg2, int* arg3)
{
  return g_base64_encode_close(arg0, arg1, arg2, arg3);
}

char* Pure_g_base64_encode(unsigned char const* arg0, unsigned long arg1)
{
  return g_base64_encode(arg0, arg1);
}

unsigned long Pure_g_base64_decode_step(char const* arg0, unsigned long arg1, unsigned char* arg2, int* arg3, unsigned int* arg4)
{
  return g_base64_decode_step(arg0, arg1, arg2, arg3, arg4);
}

unsigned char* Pure_g_base64_decode(char const* arg0, unsigned long* arg1)
{
  return g_base64_decode(arg0, arg1);
}

unsigned int Pure_g_bookmark_file_error_quark()
{
  return g_bookmark_file_error_quark();
}

GBookmarkFile* Pure_g_bookmark_file_new()
{
  return g_bookmark_file_new();
}

void Pure_g_bookmark_file_free(GBookmarkFile* arg0)
{
  return g_bookmark_file_free(arg0);
}

int Pure_g_bookmark_file_load_from_file(GBookmarkFile* arg0, char const* arg1, GError** arg2)
{
  return g_bookmark_file_load_from_file(arg0, arg1, arg2);
}

int Pure_g_bookmark_file_load_from_data(GBookmarkFile* arg0, char const* arg1, unsigned long arg2, GError** arg3)
{
  return g_bookmark_file_load_from_data(arg0, arg1, arg2, arg3);
}

int Pure_g_bookmark_file_load_from_data_dirs(GBookmarkFile* arg0, char const* arg1, char** arg2, GError** arg3)
{
  return g_bookmark_file_load_from_data_dirs(arg0, arg1, arg2, arg3);
}

char* Pure_g_bookmark_file_to_data(GBookmarkFile* arg0, unsigned long* arg1, GError** arg2)
{
  return g_bookmark_file_to_data(arg0, arg1, arg2);
}

int Pure_g_bookmark_file_to_file(GBookmarkFile* arg0, char const* arg1, GError** arg2)
{
  return g_bookmark_file_to_file(arg0, arg1, arg2);
}

void Pure_g_bookmark_file_set_title(GBookmarkFile* arg0, char const* arg1, char const* arg2)
{
  return g_bookmark_file_set_title(arg0, arg1, arg2);
}

char* Pure_g_bookmark_file_get_title(GBookmarkFile* arg0, char const* arg1, GError** arg2)
{
  return g_bookmark_file_get_title(arg0, arg1, arg2);
}

void Pure_g_bookmark_file_set_description(GBookmarkFile* arg0, char const* arg1, char const* arg2)
{
  return g_bookmark_file_set_description(arg0, arg1, arg2);
}

char* Pure_g_bookmark_file_get_description(GBookmarkFile* arg0, char const* arg1, GError** arg2)
{
  return g_bookmark_file_get_description(arg0, arg1, arg2);
}

void Pure_g_bookmark_file_set_mime_type(GBookmarkFile* arg0, char const* arg1, char const* arg2)
{
  return g_bookmark_file_set_mime_type(arg0, arg1, arg2);
}

char* Pure_g_bookmark_file_get_mime_type(GBookmarkFile* arg0, char const* arg1, GError** arg2)
{
  return g_bookmark_file_get_mime_type(arg0, arg1, arg2);
}

void Pure_g_bookmark_file_set_groups(GBookmarkFile* arg0, char const* arg1, char const** arg2, unsigned long arg3)
{
  return g_bookmark_file_set_groups(arg0, arg1, arg2, arg3);
}

void Pure_g_bookmark_file_add_group(GBookmarkFile* arg0, char const* arg1, char const* arg2)
{
  return g_bookmark_file_add_group(arg0, arg1, arg2);
}

int Pure_g_bookmark_file_has_group(GBookmarkFile* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return g_bookmark_file_has_group(arg0, arg1, arg2, arg3);
}

char** Pure_g_bookmark_file_get_groups(GBookmarkFile* arg0, char const* arg1, unsigned long* arg2, GError** arg3)
{
  return g_bookmark_file_get_groups(arg0, arg1, arg2, arg3);
}

void Pure_g_bookmark_file_add_application(GBookmarkFile* arg0, char const* arg1, char const* arg2, char const* arg3)
{
  return g_bookmark_file_add_application(arg0, arg1, arg2, arg3);
}

int Pure_g_bookmark_file_has_application(GBookmarkFile* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return g_bookmark_file_has_application(arg0, arg1, arg2, arg3);
}

char** Pure_g_bookmark_file_get_applications(GBookmarkFile* arg0, char const* arg1, unsigned long* arg2, GError** arg3)
{
  return g_bookmark_file_get_applications(arg0, arg1, arg2, arg3);
}

int Pure_g_bookmark_file_set_app_info(GBookmarkFile* arg0, char const* arg1, char const* arg2, char const* arg3, int arg4, long arg5, GError** arg6)
{
  return g_bookmark_file_set_app_info(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

int Pure_g_bookmark_file_get_app_info(GBookmarkFile* arg0, char const* arg1, char const* arg2, char** arg3, unsigned int* arg4, long* arg5, GError** arg6)
{
  return g_bookmark_file_get_app_info(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_g_bookmark_file_set_is_private(GBookmarkFile* arg0, char const* arg1, int arg2)
{
  return g_bookmark_file_set_is_private(arg0, arg1, arg2);
}

int Pure_g_bookmark_file_get_is_private(GBookmarkFile* arg0, char const* arg1, GError** arg2)
{
  return g_bookmark_file_get_is_private(arg0, arg1, arg2);
}

void Pure_g_bookmark_file_set_icon(GBookmarkFile* arg0, char const* arg1, char const* arg2, char const* arg3)
{
  return g_bookmark_file_set_icon(arg0, arg1, arg2, arg3);
}

int Pure_g_bookmark_file_get_icon(GBookmarkFile* arg0, char const* arg1, char** arg2, char** arg3, GError** arg4)
{
  return g_bookmark_file_get_icon(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_bookmark_file_set_added(GBookmarkFile* arg0, char const* arg1, long arg2)
{
  return g_bookmark_file_set_added(arg0, arg1, arg2);
}

long Pure_g_bookmark_file_get_added(GBookmarkFile* arg0, char const* arg1, GError** arg2)
{
  return g_bookmark_file_get_added(arg0, arg1, arg2);
}

void Pure_g_bookmark_file_set_modified(GBookmarkFile* arg0, char const* arg1, long arg2)
{
  return g_bookmark_file_set_modified(arg0, arg1, arg2);
}

long Pure_g_bookmark_file_get_modified(GBookmarkFile* arg0, char const* arg1, GError** arg2)
{
  return g_bookmark_file_get_modified(arg0, arg1, arg2);
}

void Pure_g_bookmark_file_set_visited(GBookmarkFile* arg0, char const* arg1, long arg2)
{
  return g_bookmark_file_set_visited(arg0, arg1, arg2);
}

long Pure_g_bookmark_file_get_visited(GBookmarkFile* arg0, char const* arg1, GError** arg2)
{
  return g_bookmark_file_get_visited(arg0, arg1, arg2);
}

int Pure_g_bookmark_file_has_item(GBookmarkFile* arg0, char const* arg1)
{
  return g_bookmark_file_has_item(arg0, arg1);
}

int Pure_g_bookmark_file_get_size(GBookmarkFile* arg0)
{
  return g_bookmark_file_get_size(arg0);
}

char** Pure_g_bookmark_file_get_uris(GBookmarkFile* arg0, unsigned long* arg1)
{
  return g_bookmark_file_get_uris(arg0, arg1);
}

int Pure_g_bookmark_file_remove_group(GBookmarkFile* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return g_bookmark_file_remove_group(arg0, arg1, arg2, arg3);
}

int Pure_g_bookmark_file_remove_application(GBookmarkFile* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return g_bookmark_file_remove_application(arg0, arg1, arg2, arg3);
}

int Pure_g_bookmark_file_remove_item(GBookmarkFile* arg0, char const* arg1, GError** arg2)
{
  return g_bookmark_file_remove_item(arg0, arg1, arg2);
}

int Pure_g_bookmark_file_move_item(GBookmarkFile* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return g_bookmark_file_move_item(arg0, arg1, arg2, arg3);
}

void* Pure_g_slice_alloc(unsigned long arg0)
{
  return g_slice_alloc(arg0);
}

void* Pure_g_slice_alloc0(unsigned long arg0)
{
  return g_slice_alloc0(arg0);
}

void* Pure_g_slice_copy(unsigned long arg0, void const* arg1)
{
  return g_slice_copy(arg0, arg1);
}

void Pure_g_slice_free1(unsigned long arg0, void* arg1)
{
  return g_slice_free1(arg0, arg1);
}

void Pure_g_slice_free_chain_with_offset(unsigned long arg0, void* arg1, unsigned long arg2)
{
  return g_slice_free_chain_with_offset(arg0, arg1, arg2);
}

void Pure_g_slice_set_config(unsigned int arg0, long arg1)
{
  return g_slice_set_config(arg0, arg1);
}

long Pure_g_slice_get_config(unsigned int arg0)
{
  return g_slice_get_config(arg0);
}

long* Pure_g_slice_get_config_state(unsigned int arg0, long arg1, unsigned int* arg2)
{
  return g_slice_get_config_state(arg0, arg1, arg2);
}

void* Pure_g_malloc(unsigned long arg0)
{
  return g_malloc(arg0);
}

void* Pure_g_malloc0(unsigned long arg0)
{
  return g_malloc0(arg0);
}

void* Pure_g_realloc(void* arg0, unsigned long arg1)
{
  return g_realloc(arg0, arg1);
}

void Pure_g_free(void* arg0)
{
  return g_free(arg0);
}

void* Pure_g_try_malloc(unsigned long arg0)
{
  return g_try_malloc(arg0);
}

void* Pure_g_try_malloc0(unsigned long arg0)
{
  return g_try_malloc0(arg0);
}

void* Pure_g_try_realloc(void* arg0, unsigned long arg1)
{
  return g_try_realloc(arg0, arg1);
}

void Pure_g_mem_set_vtable(GMemVTable* arg0)
{
  return g_mem_set_vtable(arg0);
}

int Pure_g_mem_is_system_malloc()
{
  return g_mem_is_system_malloc();
}

void Pure_g_mem_profile()
{
  return g_mem_profile();
}

GMemChunk* Pure_g_mem_chunk_new(char const* arg0, int arg1, unsigned long arg2, int arg3)
{
  return g_mem_chunk_new(arg0, arg1, arg2, arg3);
}

void Pure_g_mem_chunk_destroy(GMemChunk* arg0)
{
  return g_mem_chunk_destroy(arg0);
}

void* Pure_g_mem_chunk_alloc(GMemChunk* arg0)
{
  return g_mem_chunk_alloc(arg0);
}

void* Pure_g_mem_chunk_alloc0(GMemChunk* arg0)
{
  return g_mem_chunk_alloc0(arg0);
}

void Pure_g_mem_chunk_free(GMemChunk* arg0, void* arg1)
{
  return g_mem_chunk_free(arg0, arg1);
}

void Pure_g_mem_chunk_clean(GMemChunk* arg0)
{
  return g_mem_chunk_clean(arg0);
}

void Pure_g_mem_chunk_reset(GMemChunk* arg0)
{
  return g_mem_chunk_reset(arg0);
}

void Pure_g_mem_chunk_print(GMemChunk* arg0)
{
  return g_mem_chunk_print(arg0);
}

void Pure_g_mem_chunk_info()
{
  return g_mem_chunk_info();
}

void Pure_g_blow_chunks()
{
  return g_blow_chunks();
}

GAllocator* Pure_g_allocator_new(char const* arg0, unsigned int arg1)
{
  return g_allocator_new(arg0, arg1);
}

void Pure_g_allocator_free(GAllocator* arg0)
{
  return g_allocator_free(arg0);
}

GList* Pure_g_list_alloc()
{
  return g_list_alloc();
}

void Pure_g_list_free(GList* arg0)
{
  return g_list_free(arg0);
}

void Pure_g_list_free_1(GList* arg0)
{
  return g_list_free_1(arg0);
}

GList* Pure_g_list_append(GList* arg0, void* arg1)
{
  return g_list_append(arg0, arg1);
}

GList* Pure_g_list_prepend(GList* arg0, void* arg1)
{
  return g_list_prepend(arg0, arg1);
}

GList* Pure_g_list_insert(GList* arg0, void* arg1, int arg2)
{
  return g_list_insert(arg0, arg1, arg2);
}

GList* Pure_g_list_insert_sorted(GList* arg0, void* arg1, void* arg2)
{
  return g_list_insert_sorted(arg0, arg1, arg2);
}

GList* Pure_g_list_insert_sorted_with_data(GList* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_list_insert_sorted_with_data(arg0, arg1, arg2, arg3);
}

GList* Pure_g_list_insert_before(GList* arg0, GList* arg1, void* arg2)
{
  return g_list_insert_before(arg0, arg1, arg2);
}

GList* Pure_g_list_concat(GList* arg0, GList* arg1)
{
  return g_list_concat(arg0, arg1);
}

GList* Pure_g_list_remove(GList* arg0, void const* arg1)
{
  return g_list_remove(arg0, arg1);
}

GList* Pure_g_list_remove_all(GList* arg0, void const* arg1)
{
  return g_list_remove_all(arg0, arg1);
}

GList* Pure_g_list_remove_link(GList* arg0, GList* arg1)
{
  return g_list_remove_link(arg0, arg1);
}

GList* Pure_g_list_delete_link(GList* arg0, GList* arg1)
{
  return g_list_delete_link(arg0, arg1);
}

GList* Pure_g_list_reverse(GList* arg0)
{
  return g_list_reverse(arg0);
}

GList* Pure_g_list_copy(GList* arg0)
{
  return g_list_copy(arg0);
}

GList* Pure_g_list_nth(GList* arg0, unsigned int arg1)
{
  return g_list_nth(arg0, arg1);
}

GList* Pure_g_list_nth_prev(GList* arg0, unsigned int arg1)
{
  return g_list_nth_prev(arg0, arg1);
}

GList* Pure_g_list_find(GList* arg0, void const* arg1)
{
  return g_list_find(arg0, arg1);
}

GList* Pure_g_list_find_custom(GList* arg0, void const* arg1, void* arg2)
{
  return g_list_find_custom(arg0, arg1, arg2);
}

int Pure_g_list_position(GList* arg0, GList* arg1)
{
  return g_list_position(arg0, arg1);
}

int Pure_g_list_index(GList* arg0, void const* arg1)
{
  return g_list_index(arg0, arg1);
}

GList* Pure_g_list_last(GList* arg0)
{
  return g_list_last(arg0);
}

GList* Pure_g_list_first(GList* arg0)
{
  return g_list_first(arg0);
}

unsigned int Pure_g_list_length(GList* arg0)
{
  return g_list_length(arg0);
}

void Pure_g_list_foreach(GList* arg0, void* arg1, void* arg2)
{
  return g_list_foreach(arg0, arg1, arg2);
}

GList* Pure_g_list_sort(GList* arg0, void* arg1)
{
  return g_list_sort(arg0, arg1);
}

GList* Pure_g_list_sort_with_data(GList* arg0, void* arg1, void* arg2)
{
  return g_list_sort_with_data(arg0, arg1, arg2);
}

void* Pure_g_list_nth_data(GList* arg0, unsigned int arg1)
{
  return g_list_nth_data(arg0, arg1);
}

void Pure_g_list_push_allocator(void* arg0)
{
  return g_list_push_allocator(arg0);
}

void Pure_g_list_pop_allocator()
{
  return g_list_pop_allocator();
}

GCache* Pure_g_cache_new(void* arg0, void* arg1, void* arg2, void* arg3, void* arg4, void* arg5, void* arg6)
{
  return g_cache_new(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_g_cache_destroy(GCache* arg0)
{
  return g_cache_destroy(arg0);
}

void* Pure_g_cache_insert(GCache* arg0, void* arg1)
{
  return g_cache_insert(arg0, arg1);
}

void Pure_g_cache_remove(GCache* arg0, void const* arg1)
{
  return g_cache_remove(arg0, arg1);
}

void Pure_g_cache_key_foreach(GCache* arg0, void* arg1, void* arg2)
{
  return g_cache_key_foreach(arg0, arg1, arg2);
}

void Pure_g_cache_value_foreach(GCache* arg0, void* arg1, void* arg2)
{
  return g_cache_value_foreach(arg0, arg1, arg2);
}

long Pure_g_checksum_type_get_length(unsigned int arg0)
{
  return g_checksum_type_get_length(arg0);
}

GChecksum* Pure_g_checksum_new(unsigned int arg0)
{
  return g_checksum_new(arg0);
}

void Pure_g_checksum_reset(GChecksum* arg0)
{
  return g_checksum_reset(arg0);
}

GChecksum* Pure_g_checksum_copy(GChecksum const* arg0)
{
  return g_checksum_copy(arg0);
}

void Pure_g_checksum_free(GChecksum* arg0)
{
  return g_checksum_free(arg0);
}

void Pure_g_checksum_update(GChecksum* arg0, unsigned char const* arg1, long arg2)
{
  return g_checksum_update(arg0, arg1, arg2);
}

char const* Pure_g_checksum_get_string(GChecksum* arg0)
{
  return g_checksum_get_string(arg0);
}

void Pure_g_checksum_get_digest(GChecksum* arg0, unsigned char* arg1, unsigned long* arg2)
{
  return g_checksum_get_digest(arg0, arg1, arg2);
}

char* Pure_g_compute_checksum_for_data(unsigned int arg0, unsigned char const* arg1, unsigned long arg2)
{
  return g_compute_checksum_for_data(arg0, arg1, arg2);
}

char* Pure_g_compute_checksum_for_string(unsigned int arg0, char const* arg1, long arg2)
{
  return g_compute_checksum_for_string(arg0, arg1, arg2);
}

GCompletion* Pure_g_completion_new(void* arg0)
{
  return g_completion_new(arg0);
}

void Pure_g_completion_add_items(GCompletion* arg0, GList* arg1)
{
  return g_completion_add_items(arg0, arg1);
}

void Pure_g_completion_remove_items(GCompletion* arg0, GList* arg1)
{
  return g_completion_remove_items(arg0, arg1);
}

void Pure_g_completion_clear_items(GCompletion* arg0)
{
  return g_completion_clear_items(arg0);
}

GList* Pure_g_completion_complete(GCompletion* arg0, char const* arg1, char** arg2)
{
  return g_completion_complete(arg0, arg1, arg2);
}

GList* Pure_g_completion_complete_utf8(GCompletion* arg0, char const* arg1, char** arg2)
{
  return g_completion_complete_utf8(arg0, arg1, arg2);
}

void Pure_g_completion_set_compare(GCompletion* arg0, void* arg1)
{
  return g_completion_set_compare(arg0, arg1);
}

void Pure_g_completion_free(GCompletion* arg0)
{
  return g_completion_free(arg0);
}

unsigned int Pure_g_convert_error_quark()
{
  return g_convert_error_quark();
}

struct _GIConv* Pure_g_iconv_open(char const* arg0, char const* arg1)
{
  return g_iconv_open(arg0, arg1);
}

unsigned long Pure_g_iconv(struct _GIConv* arg0, char** arg1, unsigned long* arg2, char** arg3, unsigned long* arg4)
{
  return g_iconv(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_iconv_close(struct _GIConv* arg0)
{
  return g_iconv_close(arg0);
}

char* Pure_g_convert(char const* arg0, long arg1, char const* arg2, char const* arg3, unsigned long* arg4, unsigned long* arg5, GError** arg6)
{
  return g_convert(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

char* Pure_g_convert_with_iconv(char const* arg0, long arg1, struct _GIConv* arg2, unsigned long* arg3, unsigned long* arg4, GError** arg5)
{
  return g_convert_with_iconv(arg0, arg1, arg2, arg3, arg4, arg5);
}

char* Pure_g_convert_with_fallback(char const* arg0, long arg1, char const* arg2, char const* arg3, char* arg4, unsigned long* arg5, unsigned long* arg6, GError** arg7)
{
  return g_convert_with_fallback(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

char* Pure_g_locale_to_utf8(char const* arg0, long arg1, unsigned long* arg2, unsigned long* arg3, GError** arg4)
{
  return g_locale_to_utf8(arg0, arg1, arg2, arg3, arg4);
}

char* Pure_g_locale_from_utf8(char const* arg0, long arg1, unsigned long* arg2, unsigned long* arg3, GError** arg4)
{
  return g_locale_from_utf8(arg0, arg1, arg2, arg3, arg4);
}

char* Pure_g_filename_to_utf8(char const* arg0, long arg1, unsigned long* arg2, unsigned long* arg3, GError** arg4)
{
  return g_filename_to_utf8(arg0, arg1, arg2, arg3, arg4);
}

char* Pure_g_filename_from_utf8(char const* arg0, long arg1, unsigned long* arg2, unsigned long* arg3, GError** arg4)
{
  return g_filename_from_utf8(arg0, arg1, arg2, arg3, arg4);
}

char* Pure_g_filename_from_uri(char const* arg0, char** arg1, GError** arg2)
{
  return g_filename_from_uri(arg0, arg1, arg2);
}

char* Pure_g_filename_to_uri(char const* arg0, char const* arg1, GError** arg2)
{
  return g_filename_to_uri(arg0, arg1, arg2);
}

char* Pure_g_filename_display_name(char const* arg0)
{
  return g_filename_display_name(arg0);
}

int Pure_g_get_filename_charsets(char const*** arg0)
{
  return g_get_filename_charsets(arg0);
}

char* Pure_g_filename_display_basename(char const* arg0)
{
  return g_filename_display_basename(arg0);
}

char** Pure_g_uri_list_extract_uris(char const* arg0)
{
  return g_uri_list_extract_uris(arg0);
}

void Pure_g_datalist_init(GData** arg0)
{
  return g_datalist_init(arg0);
}

void Pure_g_datalist_clear(GData** arg0)
{
  return g_datalist_clear(arg0);
}

void* Pure_g_datalist_id_get_data(GData** arg0, unsigned int arg1)
{
  return g_datalist_id_get_data(arg0, arg1);
}

void Pure_g_datalist_id_set_data_full(GData** arg0, unsigned int arg1, void* arg2, void* arg3)
{
  return g_datalist_id_set_data_full(arg0, arg1, arg2, arg3);
}

void* Pure_g_datalist_id_remove_no_notify(GData** arg0, unsigned int arg1)
{
  return g_datalist_id_remove_no_notify(arg0, arg1);
}

void Pure_g_datalist_foreach(GData** arg0, void* arg1, void* arg2)
{
  return g_datalist_foreach(arg0, arg1, arg2);
}

void Pure_g_datalist_set_flags(GData** arg0, unsigned int arg1)
{
  return g_datalist_set_flags(arg0, arg1);
}

void Pure_g_datalist_unset_flags(GData** arg0, unsigned int arg1)
{
  return g_datalist_unset_flags(arg0, arg1);
}

unsigned int Pure_g_datalist_get_flags(GData** arg0)
{
  return g_datalist_get_flags(arg0);
}

void Pure_g_dataset_destroy(void const* arg0)
{
  return g_dataset_destroy(arg0);
}

void* Pure_g_dataset_id_get_data(void const* arg0, unsigned int arg1)
{
  return g_dataset_id_get_data(arg0, arg1);
}

void Pure_g_dataset_id_set_data_full(void const* arg0, unsigned int arg1, void* arg2, void* arg3)
{
  return g_dataset_id_set_data_full(arg0, arg1, arg2, arg3);
}

void* Pure_g_dataset_id_remove_no_notify(void const* arg0, unsigned int arg1)
{
  return g_dataset_id_remove_no_notify(arg0, arg1);
}

void Pure_g_dataset_foreach(void const* arg0, void* arg1, void* arg2)
{
  return g_dataset_foreach(arg0, arg1, arg2);
}

GDate* Pure_g_date_new()
{
  return g_date_new();
}

GDate* Pure_g_date_new_dmy(unsigned char arg0, unsigned int arg1, unsigned short arg2)
{
  return g_date_new_dmy(arg0, arg1, arg2);
}

GDate* Pure_g_date_new_julian(unsigned int arg0)
{
  return g_date_new_julian(arg0);
}

void Pure_g_date_free(GDate* arg0)
{
  return g_date_free(arg0);
}

int Pure_g_date_valid(GDate const* arg0)
{
  return g_date_valid(arg0);
}

int Pure_g_date_valid_day(unsigned char arg0)
{
  return g_date_valid_day(arg0);
}

int Pure_g_date_valid_month(unsigned int arg0)
{
  return g_date_valid_month(arg0);
}

int Pure_g_date_valid_year(unsigned short arg0)
{
  return g_date_valid_year(arg0);
}

int Pure_g_date_valid_weekday(unsigned int arg0)
{
  return g_date_valid_weekday(arg0);
}

int Pure_g_date_valid_julian(unsigned int arg0)
{
  return g_date_valid_julian(arg0);
}

int Pure_g_date_valid_dmy(unsigned char arg0, unsigned int arg1, unsigned short arg2)
{
  return g_date_valid_dmy(arg0, arg1, arg2);
}

unsigned int Pure_g_date_get_weekday(GDate const* arg0)
{
  return g_date_get_weekday(arg0);
}

unsigned int Pure_g_date_get_month(GDate const* arg0)
{
  return g_date_get_month(arg0);
}

unsigned short Pure_g_date_get_year(GDate const* arg0)
{
  return g_date_get_year(arg0);
}

unsigned char Pure_g_date_get_day(GDate const* arg0)
{
  return g_date_get_day(arg0);
}

unsigned int Pure_g_date_get_julian(GDate const* arg0)
{
  return g_date_get_julian(arg0);
}

unsigned int Pure_g_date_get_day_of_year(GDate const* arg0)
{
  return g_date_get_day_of_year(arg0);
}

unsigned int Pure_g_date_get_monday_week_of_year(GDate const* arg0)
{
  return g_date_get_monday_week_of_year(arg0);
}

unsigned int Pure_g_date_get_sunday_week_of_year(GDate const* arg0)
{
  return g_date_get_sunday_week_of_year(arg0);
}

unsigned int Pure_g_date_get_iso8601_week_of_year(GDate const* arg0)
{
  return g_date_get_iso8601_week_of_year(arg0);
}

void Pure_g_date_clear(GDate* arg0, unsigned int arg1)
{
  return g_date_clear(arg0, arg1);
}

void Pure_g_date_set_parse(GDate* arg0, char const* arg1)
{
  return g_date_set_parse(arg0, arg1);
}

void Pure_g_date_set_time_t(GDate* arg0, long arg1)
{
  return g_date_set_time_t(arg0, arg1);
}

void Pure_g_date_set_time_val(GDate* arg0, GTimeVal* arg1)
{
  return g_date_set_time_val(arg0, arg1);
}

void Pure_g_date_set_time(GDate* arg0, int arg1)
{
  return g_date_set_time(arg0, arg1);
}

void Pure_g_date_set_month(GDate* arg0, unsigned int arg1)
{
  return g_date_set_month(arg0, arg1);
}

void Pure_g_date_set_day(GDate* arg0, unsigned char arg1)
{
  return g_date_set_day(arg0, arg1);
}

void Pure_g_date_set_year(GDate* arg0, unsigned short arg1)
{
  return g_date_set_year(arg0, arg1);
}

void Pure_g_date_set_dmy(GDate* arg0, unsigned char arg1, unsigned int arg2, unsigned short arg3)
{
  return g_date_set_dmy(arg0, arg1, arg2, arg3);
}

void Pure_g_date_set_julian(GDate* arg0, unsigned int arg1)
{
  return g_date_set_julian(arg0, arg1);
}

int Pure_g_date_is_first_of_month(GDate const* arg0)
{
  return g_date_is_first_of_month(arg0);
}

int Pure_g_date_is_last_of_month(GDate const* arg0)
{
  return g_date_is_last_of_month(arg0);
}

void Pure_g_date_add_days(GDate* arg0, unsigned int arg1)
{
  return g_date_add_days(arg0, arg1);
}

void Pure_g_date_subtract_days(GDate* arg0, unsigned int arg1)
{
  return g_date_subtract_days(arg0, arg1);
}

void Pure_g_date_add_months(GDate* arg0, unsigned int arg1)
{
  return g_date_add_months(arg0, arg1);
}

void Pure_g_date_subtract_months(GDate* arg0, unsigned int arg1)
{
  return g_date_subtract_months(arg0, arg1);
}

void Pure_g_date_add_years(GDate* arg0, unsigned int arg1)
{
  return g_date_add_years(arg0, arg1);
}

void Pure_g_date_subtract_years(GDate* arg0, unsigned int arg1)
{
  return g_date_subtract_years(arg0, arg1);
}

int Pure_g_date_is_leap_year(unsigned short arg0)
{
  return g_date_is_leap_year(arg0);
}

unsigned char Pure_g_date_get_days_in_month(unsigned int arg0, unsigned short arg1)
{
  return g_date_get_days_in_month(arg0, arg1);
}

unsigned char Pure_g_date_get_monday_weeks_in_year(unsigned short arg0)
{
  return g_date_get_monday_weeks_in_year(arg0);
}

unsigned char Pure_g_date_get_sunday_weeks_in_year(unsigned short arg0)
{
  return g_date_get_sunday_weeks_in_year(arg0);
}

int Pure_g_date_days_between(GDate const* arg0, GDate const* arg1)
{
  return g_date_days_between(arg0, arg1);
}

int Pure_g_date_compare(GDate const* arg0, GDate const* arg1)
{
  return g_date_compare(arg0, arg1);
}

void Pure_g_date_to_struct_tm(GDate const* arg0, struct tm* arg1)
{
  return g_date_to_struct_tm(arg0, arg1);
}

void Pure_g_date_clamp(GDate* arg0, GDate const* arg1, GDate const* arg2)
{
  return g_date_clamp(arg0, arg1, arg2);
}

void Pure_g_date_order(GDate* arg0, GDate* arg1)
{
  return g_date_order(arg0, arg1);
}

unsigned long Pure_g_date_strftime(char* arg0, unsigned long arg1, char const* arg2, GDate const* arg3)
{
  return g_date_strftime(arg0, arg1, arg2, arg3);
}

GDir* Pure_g_dir_open(char const* arg0, unsigned int arg1, GError** arg2)
{
  return g_dir_open(arg0, arg1, arg2);
}

char const* Pure_g_dir_read_name(GDir* arg0)
{
  return g_dir_read_name(arg0);
}

void Pure_g_dir_rewind(GDir* arg0)
{
  return g_dir_rewind(arg0);
}

void Pure_g_dir_close(GDir* arg0)
{
  return g_dir_close(arg0);
}

unsigned int Pure_g_file_error_quark()
{
  return g_file_error_quark();
}

unsigned int Pure_g_file_error_from_errno(int arg0)
{
  return g_file_error_from_errno(arg0);
}

int Pure_g_file_test(char const* arg0, unsigned int arg1)
{
  return g_file_test(arg0, arg1);
}

int Pure_g_file_get_contents(char const* arg0, char** arg1, unsigned long* arg2, GError** arg3)
{
  return g_file_get_contents(arg0, arg1, arg2, arg3);
}

int Pure_g_file_set_contents(char const* arg0, char const* arg1, long arg2, GError** arg3)
{
  return g_file_set_contents(arg0, arg1, arg2, arg3);
}

char* Pure_g_file_read_link(char const* arg0, GError** arg1)
{
  return g_file_read_link(arg0, arg1);
}

int Pure_g_mkstemp(char* arg0)
{
  return g_mkstemp(arg0);
}

int Pure_g_file_open_tmp(char const* arg0, char** arg1, GError** arg2)
{
  return g_file_open_tmp(arg0, arg1, arg2);
}

char* Pure_g_format_size_for_display(long arg0)
{
  return g_format_size_for_display(arg0);
}

char* Pure_g_build_path(char const* arg0, char const* arg1)
{
  return g_build_path(arg0, arg1);
}

char* Pure_g_build_pathv(char const* arg0, char** arg1)
{
  return g_build_pathv(arg0, arg1);
}

char* Pure_g_build_filename(char const* arg0)
{
  return g_build_filename(arg0);
}

char* Pure_g_build_filenamev(char** arg0)
{
  return g_build_filenamev(arg0);
}

int Pure_g_mkdir_with_parents(char const* arg0, int arg1)
{
  return g_mkdir_with_parents(arg0, arg1);
}

GHashTable* Pure_g_hash_table_new(void* arg0, void* arg1)
{
  return g_hash_table_new(arg0, arg1);
}

GHashTable* Pure_g_hash_table_new_full(void* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_hash_table_new_full(arg0, arg1, arg2, arg3);
}

void Pure_g_hash_table_destroy(GHashTable* arg0)
{
  return g_hash_table_destroy(arg0);
}

void Pure_g_hash_table_insert(GHashTable* arg0, void* arg1, void* arg2)
{
  return g_hash_table_insert(arg0, arg1, arg2);
}

void Pure_g_hash_table_replace(GHashTable* arg0, void* arg1, void* arg2)
{
  return g_hash_table_replace(arg0, arg1, arg2);
}

int Pure_g_hash_table_remove(GHashTable* arg0, void const* arg1)
{
  return g_hash_table_remove(arg0, arg1);
}

void Pure_g_hash_table_remove_all(GHashTable* arg0)
{
  return g_hash_table_remove_all(arg0);
}

int Pure_g_hash_table_steal(GHashTable* arg0, void const* arg1)
{
  return g_hash_table_steal(arg0, arg1);
}

void Pure_g_hash_table_steal_all(GHashTable* arg0)
{
  return g_hash_table_steal_all(arg0);
}

void* Pure_g_hash_table_lookup(GHashTable* arg0, void const* arg1)
{
  return g_hash_table_lookup(arg0, arg1);
}

int Pure_g_hash_table_lookup_extended(GHashTable* arg0, void const* arg1, void** arg2, void** arg3)
{
  return g_hash_table_lookup_extended(arg0, arg1, arg2, arg3);
}

void Pure_g_hash_table_foreach(GHashTable* arg0, void* arg1, void* arg2)
{
  return g_hash_table_foreach(arg0, arg1, arg2);
}

void* Pure_g_hash_table_find(GHashTable* arg0, void* arg1, void* arg2)
{
  return g_hash_table_find(arg0, arg1, arg2);
}

unsigned int Pure_g_hash_table_foreach_remove(GHashTable* arg0, void* arg1, void* arg2)
{
  return g_hash_table_foreach_remove(arg0, arg1, arg2);
}

unsigned int Pure_g_hash_table_foreach_steal(GHashTable* arg0, void* arg1, void* arg2)
{
  return g_hash_table_foreach_steal(arg0, arg1, arg2);
}

unsigned int Pure_g_hash_table_size(GHashTable* arg0)
{
  return g_hash_table_size(arg0);
}

GList* Pure_g_hash_table_get_keys(GHashTable* arg0)
{
  return g_hash_table_get_keys(arg0);
}

GList* Pure_g_hash_table_get_values(GHashTable* arg0)
{
  return g_hash_table_get_values(arg0);
}

void Pure_g_hash_table_iter_init(GHashTableIter* arg0, GHashTable* arg1)
{
  return g_hash_table_iter_init(arg0, arg1);
}

int Pure_g_hash_table_iter_next(GHashTableIter* arg0, void** arg1, void** arg2)
{
  return g_hash_table_iter_next(arg0, arg1, arg2);
}

GHashTable* Pure_g_hash_table_iter_get_hash_table(GHashTableIter* arg0)
{
  return g_hash_table_iter_get_hash_table(arg0);
}

void Pure_g_hash_table_iter_remove(GHashTableIter* arg0)
{
  return g_hash_table_iter_remove(arg0);
}

void Pure_g_hash_table_iter_steal(GHashTableIter* arg0)
{
  return g_hash_table_iter_steal(arg0);
}

GHashTable* Pure_g_hash_table_ref(GHashTable* arg0)
{
  return g_hash_table_ref(arg0);
}

void Pure_g_hash_table_unref(GHashTable* arg0)
{
  return g_hash_table_unref(arg0);
}

int Pure_g_str_equal(void const* arg0, void const* arg1)
{
  return g_str_equal(arg0, arg1);
}

unsigned int Pure_g_str_hash(void const* arg0)
{
  return g_str_hash(arg0);
}

int Pure_g_int_equal(void const* arg0, void const* arg1)
{
  return g_int_equal(arg0, arg1);
}

unsigned int Pure_g_int_hash(void const* arg0)
{
  return g_int_hash(arg0);
}

unsigned int Pure_g_direct_hash(void const* arg0)
{
  return g_direct_hash(arg0);
}

int Pure_g_direct_equal(void const* arg0, void const* arg1)
{
  return g_direct_equal(arg0, arg1);
}

void Pure_g_hook_list_init(GHookList* arg0, unsigned int arg1)
{
  return g_hook_list_init(arg0, arg1);
}

void Pure_g_hook_list_clear(GHookList* arg0)
{
  return g_hook_list_clear(arg0);
}

GHook* Pure_g_hook_alloc(GHookList* arg0)
{
  return g_hook_alloc(arg0);
}

void Pure_g_hook_free(GHookList* arg0, GHook* arg1)
{
  return g_hook_free(arg0, arg1);
}

GHook* Pure_g_hook_ref(GHookList* arg0, GHook* arg1)
{
  return g_hook_ref(arg0, arg1);
}

void Pure_g_hook_unref(GHookList* arg0, GHook* arg1)
{
  return g_hook_unref(arg0, arg1);
}

int Pure_g_hook_destroy(GHookList* arg0, unsigned long arg1)
{
  return g_hook_destroy(arg0, arg1);
}

void Pure_g_hook_destroy_link(GHookList* arg0, GHook* arg1)
{
  return g_hook_destroy_link(arg0, arg1);
}

void Pure_g_hook_prepend(GHookList* arg0, GHook* arg1)
{
  return g_hook_prepend(arg0, arg1);
}

void Pure_g_hook_insert_before(GHookList* arg0, GHook* arg1, GHook* arg2)
{
  return g_hook_insert_before(arg0, arg1, arg2);
}

void Pure_g_hook_insert_sorted(GHookList* arg0, GHook* arg1, void* arg2)
{
  return g_hook_insert_sorted(arg0, arg1, arg2);
}

GHook* Pure_g_hook_get(GHookList* arg0, unsigned long arg1)
{
  return g_hook_get(arg0, arg1);
}

GHook* Pure_g_hook_find(GHookList* arg0, int arg1, void* arg2, void* arg3)
{
  return g_hook_find(arg0, arg1, arg2, arg3);
}

GHook* Pure_g_hook_find_data(GHookList* arg0, int arg1, void* arg2)
{
  return g_hook_find_data(arg0, arg1, arg2);
}

GHook* Pure_g_hook_find_func(GHookList* arg0, int arg1, void* arg2)
{
  return g_hook_find_func(arg0, arg1, arg2);
}

GHook* Pure_g_hook_find_func_data(GHookList* arg0, int arg1, void* arg2, void* arg3)
{
  return g_hook_find_func_data(arg0, arg1, arg2, arg3);
}

GHook* Pure_g_hook_first_valid(GHookList* arg0, int arg1)
{
  return g_hook_first_valid(arg0, arg1);
}

GHook* Pure_g_hook_next_valid(GHookList* arg0, GHook* arg1, int arg2)
{
  return g_hook_next_valid(arg0, arg1, arg2);
}

int Pure_g_hook_compare_ids(GHook* arg0, GHook* arg1)
{
  return g_hook_compare_ids(arg0, arg1);
}

void Pure_g_hook_list_invoke(GHookList* arg0, int arg1)
{
  return g_hook_list_invoke(arg0, arg1);
}

void Pure_g_hook_list_invoke_check(GHookList* arg0, int arg1)
{
  return g_hook_list_invoke_check(arg0, arg1);
}

void Pure_g_hook_list_marshal(GHookList* arg0, int arg1, void* arg2, void* arg3)
{
  return g_hook_list_marshal(arg0, arg1, arg2, arg3);
}

void Pure_g_hook_list_marshal_check(GHookList* arg0, int arg1, void* arg2, void* arg3)
{
  return g_hook_list_marshal_check(arg0, arg1, arg2, arg3);
}

GSList* Pure_g_slist_alloc()
{
  return g_slist_alloc();
}

void Pure_g_slist_free(GSList* arg0)
{
  return g_slist_free(arg0);
}

void Pure_g_slist_free_1(GSList* arg0)
{
  return g_slist_free_1(arg0);
}

GSList* Pure_g_slist_append(GSList* arg0, void* arg1)
{
  return g_slist_append(arg0, arg1);
}

GSList* Pure_g_slist_prepend(GSList* arg0, void* arg1)
{
  return g_slist_prepend(arg0, arg1);
}

GSList* Pure_g_slist_insert(GSList* arg0, void* arg1, int arg2)
{
  return g_slist_insert(arg0, arg1, arg2);
}

GSList* Pure_g_slist_insert_sorted(GSList* arg0, void* arg1, void* arg2)
{
  return g_slist_insert_sorted(arg0, arg1, arg2);
}

GSList* Pure_g_slist_insert_sorted_with_data(GSList* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_slist_insert_sorted_with_data(arg0, arg1, arg2, arg3);
}

GSList* Pure_g_slist_insert_before(GSList* arg0, GSList* arg1, void* arg2)
{
  return g_slist_insert_before(arg0, arg1, arg2);
}

GSList* Pure_g_slist_concat(GSList* arg0, GSList* arg1)
{
  return g_slist_concat(arg0, arg1);
}

GSList* Pure_g_slist_remove(GSList* arg0, void const* arg1)
{
  return g_slist_remove(arg0, arg1);
}

GSList* Pure_g_slist_remove_all(GSList* arg0, void const* arg1)
{
  return g_slist_remove_all(arg0, arg1);
}

GSList* Pure_g_slist_remove_link(GSList* arg0, GSList* arg1)
{
  return g_slist_remove_link(arg0, arg1);
}

GSList* Pure_g_slist_delete_link(GSList* arg0, GSList* arg1)
{
  return g_slist_delete_link(arg0, arg1);
}

GSList* Pure_g_slist_reverse(GSList* arg0)
{
  return g_slist_reverse(arg0);
}

GSList* Pure_g_slist_copy(GSList* arg0)
{
  return g_slist_copy(arg0);
}

GSList* Pure_g_slist_nth(GSList* arg0, unsigned int arg1)
{
  return g_slist_nth(arg0, arg1);
}

GSList* Pure_g_slist_find(GSList* arg0, void const* arg1)
{
  return g_slist_find(arg0, arg1);
}

GSList* Pure_g_slist_find_custom(GSList* arg0, void const* arg1, void* arg2)
{
  return g_slist_find_custom(arg0, arg1, arg2);
}

int Pure_g_slist_position(GSList* arg0, GSList* arg1)
{
  return g_slist_position(arg0, arg1);
}

int Pure_g_slist_index(GSList* arg0, void const* arg1)
{
  return g_slist_index(arg0, arg1);
}

GSList* Pure_g_slist_last(GSList* arg0)
{
  return g_slist_last(arg0);
}

unsigned int Pure_g_slist_length(GSList* arg0)
{
  return g_slist_length(arg0);
}

void Pure_g_slist_foreach(GSList* arg0, void* arg1, void* arg2)
{
  return g_slist_foreach(arg0, arg1, arg2);
}

GSList* Pure_g_slist_sort(GSList* arg0, void* arg1)
{
  return g_slist_sort(arg0, arg1);
}

GSList* Pure_g_slist_sort_with_data(GSList* arg0, void* arg1, void* arg2)
{
  return g_slist_sort_with_data(arg0, arg1, arg2);
}

void* Pure_g_slist_nth_data(GSList* arg0, unsigned int arg1)
{
  return g_slist_nth_data(arg0, arg1);
}

void Pure_g_slist_push_allocator(void* arg0)
{
  return g_slist_push_allocator(arg0);
}

void Pure_g_slist_pop_allocator()
{
  return g_slist_pop_allocator();
}

GMainContext* Pure_g_main_context_new()
{
  return g_main_context_new();
}

GMainContext* Pure_g_main_context_ref(GMainContext* arg0)
{
  return g_main_context_ref(arg0);
}

void Pure_g_main_context_unref(GMainContext* arg0)
{
  return g_main_context_unref(arg0);
}

GMainContext* Pure_g_main_context_default()
{
  return g_main_context_default();
}

int Pure_g_main_context_iteration(GMainContext* arg0, int arg1)
{
  return g_main_context_iteration(arg0, arg1);
}

int Pure_g_main_context_pending(GMainContext* arg0)
{
  return g_main_context_pending(arg0);
}

GSource* Pure_g_main_context_find_source_by_id(GMainContext* arg0, unsigned int arg1)
{
  return g_main_context_find_source_by_id(arg0, arg1);
}

GSource* Pure_g_main_context_find_source_by_user_data(GMainContext* arg0, void* arg1)
{
  return g_main_context_find_source_by_user_data(arg0, arg1);
}

GSource* Pure_g_main_context_find_source_by_funcs_user_data(GMainContext* arg0, GSourceFuncs* arg1, void* arg2)
{
  return g_main_context_find_source_by_funcs_user_data(arg0, arg1, arg2);
}

void Pure_g_main_context_wakeup(GMainContext* arg0)
{
  return g_main_context_wakeup(arg0);
}

int Pure_g_main_context_acquire(GMainContext* arg0)
{
  return g_main_context_acquire(arg0);
}

void Pure_g_main_context_release(GMainContext* arg0)
{
  return g_main_context_release(arg0);
}

int Pure_g_main_context_is_owner(GMainContext* arg0)
{
  return g_main_context_is_owner(arg0);
}

int Pure_g_main_context_wait(GMainContext* arg0, GCond* arg1, GMutex* arg2)
{
  return g_main_context_wait(arg0, arg1, arg2);
}

int Pure_g_main_context_prepare(GMainContext* arg0, int* arg1)
{
  return g_main_context_prepare(arg0, arg1);
}

int Pure_g_main_context_query(GMainContext* arg0, int arg1, int* arg2, GPollFD* arg3, int arg4)
{
  return g_main_context_query(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_main_context_check(GMainContext* arg0, int arg1, GPollFD* arg2, int arg3)
{
  return g_main_context_check(arg0, arg1, arg2, arg3);
}

void Pure_g_main_context_dispatch(GMainContext* arg0)
{
  return g_main_context_dispatch(arg0);
}

void Pure_g_main_context_set_poll_func(GMainContext* arg0, void* arg1)
{
  return g_main_context_set_poll_func(arg0, arg1);
}

void* Pure_g_main_context_get_poll_func(GMainContext* arg0)
{
  return g_main_context_get_poll_func(arg0);
}

void Pure_g_main_context_add_poll(GMainContext* arg0, GPollFD* arg1, int arg2)
{
  return g_main_context_add_poll(arg0, arg1, arg2);
}

void Pure_g_main_context_remove_poll(GMainContext* arg0, GPollFD* arg1)
{
  return g_main_context_remove_poll(arg0, arg1);
}

int Pure_g_main_depth()
{
  return g_main_depth();
}

GSource* Pure_g_main_current_source()
{
  return g_main_current_source();
}

GMainLoop* Pure_g_main_loop_new(GMainContext* arg0, int arg1)
{
  return g_main_loop_new(arg0, arg1);
}

void Pure_g_main_loop_run(GMainLoop* arg0)
{
  return g_main_loop_run(arg0);
}

void Pure_g_main_loop_quit(GMainLoop* arg0)
{
  return g_main_loop_quit(arg0);
}

GMainLoop* Pure_g_main_loop_ref(GMainLoop* arg0)
{
  return g_main_loop_ref(arg0);
}

void Pure_g_main_loop_unref(GMainLoop* arg0)
{
  return g_main_loop_unref(arg0);
}

int Pure_g_main_loop_is_running(GMainLoop* arg0)
{
  return g_main_loop_is_running(arg0);
}

GMainContext* Pure_g_main_loop_get_context(GMainLoop* arg0)
{
  return g_main_loop_get_context(arg0);
}

GSource* Pure_g_source_new(GSourceFuncs* arg0, unsigned int arg1)
{
  return g_source_new(arg0, arg1);
}

GSource* Pure_g_source_ref(GSource* arg0)
{
  return g_source_ref(arg0);
}

void Pure_g_source_unref(GSource* arg0)
{
  return g_source_unref(arg0);
}

unsigned int Pure_g_source_attach(GSource* arg0, GMainContext* arg1)
{
  return g_source_attach(arg0, arg1);
}

void Pure_g_source_destroy(GSource* arg0)
{
  return g_source_destroy(arg0);
}

void Pure_g_source_set_priority(GSource* arg0, int arg1)
{
  return g_source_set_priority(arg0, arg1);
}

int Pure_g_source_get_priority(GSource* arg0)
{
  return g_source_get_priority(arg0);
}

void Pure_g_source_set_can_recurse(GSource* arg0, int arg1)
{
  return g_source_set_can_recurse(arg0, arg1);
}

int Pure_g_source_get_can_recurse(GSource* arg0)
{
  return g_source_get_can_recurse(arg0);
}

unsigned int Pure_g_source_get_id(GSource* arg0)
{
  return g_source_get_id(arg0);
}

GMainContext* Pure_g_source_get_context(GSource* arg0)
{
  return g_source_get_context(arg0);
}

void Pure_g_source_set_callback(GSource* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_source_set_callback(arg0, arg1, arg2, arg3);
}

void Pure_g_source_set_funcs(GSource* arg0, GSourceFuncs* arg1)
{
  return g_source_set_funcs(arg0, arg1);
}

int Pure_g_source_is_destroyed(GSource* arg0)
{
  return g_source_is_destroyed(arg0);
}

void Pure_g_source_set_callback_indirect(GSource* arg0, void* arg1, GSourceCallbackFuncs* arg2)
{
  return g_source_set_callback_indirect(arg0, arg1, arg2);
}

void Pure_g_source_add_poll(GSource* arg0, GPollFD* arg1)
{
  return g_source_add_poll(arg0, arg1);
}

void Pure_g_source_remove_poll(GSource* arg0, GPollFD* arg1)
{
  return g_source_remove_poll(arg0, arg1);
}

void Pure_g_source_get_current_time(GSource* arg0, GTimeVal* arg1)
{
  return g_source_get_current_time(arg0, arg1);
}

GSource* Pure_g_idle_source_new()
{
  return g_idle_source_new();
}

GSource* Pure_g_child_watch_source_new(int arg0)
{
  return g_child_watch_source_new(arg0);
}

GSource* Pure_g_timeout_source_new(unsigned int arg0)
{
  return g_timeout_source_new(arg0);
}

GSource* Pure_g_timeout_source_new_seconds(unsigned int arg0)
{
  return g_timeout_source_new_seconds(arg0);
}

void Pure_g_get_current_time(GTimeVal* arg0)
{
  return g_get_current_time(arg0);
}

int Pure_g_source_remove(unsigned int arg0)
{
  return g_source_remove(arg0);
}

int Pure_g_source_remove_by_user_data(void* arg0)
{
  return g_source_remove_by_user_data(arg0);
}

int Pure_g_source_remove_by_funcs_user_data(GSourceFuncs* arg0, void* arg1)
{
  return g_source_remove_by_funcs_user_data(arg0, arg1);
}

unsigned int Pure_g_timeout_add_full(int arg0, unsigned int arg1, void* arg2, void* arg3, void* arg4)
{
  return g_timeout_add_full(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_g_timeout_add(unsigned int arg0, void* arg1, void* arg2)
{
  return g_timeout_add(arg0, arg1, arg2);
}

unsigned int Pure_g_timeout_add_seconds_full(int arg0, unsigned int arg1, void* arg2, void* arg3, void* arg4)
{
  return g_timeout_add_seconds_full(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_g_timeout_add_seconds(unsigned int arg0, void* arg1, void* arg2)
{
  return g_timeout_add_seconds(arg0, arg1, arg2);
}

unsigned int Pure_g_child_watch_add_full(int arg0, int arg1, void* arg2, void* arg3, void* arg4)
{
  return g_child_watch_add_full(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_g_child_watch_add(int arg0, void* arg1, void* arg2)
{
  return g_child_watch_add(arg0, arg1, arg2);
}

unsigned int Pure_g_idle_add(void* arg0, void* arg1)
{
  return g_idle_add(arg0, arg1);
}

unsigned int Pure_g_idle_add_full(int arg0, void* arg1, void* arg2, void* arg3)
{
  return g_idle_add_full(arg0, arg1, arg2, arg3);
}

int Pure_g_idle_remove_by_data(void* arg0)
{
  return g_idle_remove_by_data(arg0);
}

int Pure_g_get_charset(char const** arg0)
{
  return g_get_charset(arg0);
}

int Pure_g_unichar_isalnum(unsigned int arg0)
{
  return g_unichar_isalnum(arg0);
}

int Pure_g_unichar_isalpha(unsigned int arg0)
{
  return g_unichar_isalpha(arg0);
}

int Pure_g_unichar_iscntrl(unsigned int arg0)
{
  return g_unichar_iscntrl(arg0);
}

int Pure_g_unichar_isdigit(unsigned int arg0)
{
  return g_unichar_isdigit(arg0);
}

int Pure_g_unichar_isgraph(unsigned int arg0)
{
  return g_unichar_isgraph(arg0);
}

int Pure_g_unichar_islower(unsigned int arg0)
{
  return g_unichar_islower(arg0);
}

int Pure_g_unichar_isprint(unsigned int arg0)
{
  return g_unichar_isprint(arg0);
}

int Pure_g_unichar_ispunct(unsigned int arg0)
{
  return g_unichar_ispunct(arg0);
}

int Pure_g_unichar_isspace(unsigned int arg0)
{
  return g_unichar_isspace(arg0);
}

int Pure_g_unichar_isupper(unsigned int arg0)
{
  return g_unichar_isupper(arg0);
}

int Pure_g_unichar_isxdigit(unsigned int arg0)
{
  return g_unichar_isxdigit(arg0);
}

int Pure_g_unichar_istitle(unsigned int arg0)
{
  return g_unichar_istitle(arg0);
}

int Pure_g_unichar_isdefined(unsigned int arg0)
{
  return g_unichar_isdefined(arg0);
}

int Pure_g_unichar_iswide(unsigned int arg0)
{
  return g_unichar_iswide(arg0);
}

int Pure_g_unichar_iswide_cjk(unsigned int arg0)
{
  return g_unichar_iswide_cjk(arg0);
}

int Pure_g_unichar_iszerowidth(unsigned int arg0)
{
  return g_unichar_iszerowidth(arg0);
}

int Pure_g_unichar_ismark(unsigned int arg0)
{
  return g_unichar_ismark(arg0);
}

unsigned int Pure_g_unichar_toupper(unsigned int arg0)
{
  return g_unichar_toupper(arg0);
}

unsigned int Pure_g_unichar_tolower(unsigned int arg0)
{
  return g_unichar_tolower(arg0);
}

unsigned int Pure_g_unichar_totitle(unsigned int arg0)
{
  return g_unichar_totitle(arg0);
}

int Pure_g_unichar_digit_value(unsigned int arg0)
{
  return g_unichar_digit_value(arg0);
}

int Pure_g_unichar_xdigit_value(unsigned int arg0)
{
  return g_unichar_xdigit_value(arg0);
}

unsigned int Pure_g_unichar_type(unsigned int arg0)
{
  return g_unichar_type(arg0);
}

unsigned int Pure_g_unichar_break_type(unsigned int arg0)
{
  return g_unichar_break_type(arg0);
}

int Pure_g_unichar_combining_class(unsigned int arg0)
{
  return g_unichar_combining_class(arg0);
}

void Pure_g_unicode_canonical_ordering(unsigned int* arg0, unsigned long arg1)
{
  return g_unicode_canonical_ordering(arg0, arg1);
}

unsigned int* Pure_g_unicode_canonical_decomposition(unsigned int arg0, unsigned long* arg1)
{
  return g_unicode_canonical_decomposition(arg0, arg1);
}

unsigned int Pure_g_utf8_get_char(char const* arg0)
{
  return g_utf8_get_char(arg0);
}

unsigned int Pure_g_utf8_get_char_validated(char const* arg0, long arg1)
{
  return g_utf8_get_char_validated(arg0, arg1);
}

char* Pure_g_utf8_offset_to_pointer(char const* arg0, long arg1)
{
  return g_utf8_offset_to_pointer(arg0, arg1);
}

long Pure_g_utf8_pointer_to_offset(char const* arg0, char const* arg1)
{
  return g_utf8_pointer_to_offset(arg0, arg1);
}

char* Pure_g_utf8_prev_char(char const* arg0)
{
  return g_utf8_prev_char(arg0);
}

char* Pure_g_utf8_find_next_char(char const* arg0, char const* arg1)
{
  return g_utf8_find_next_char(arg0, arg1);
}

char* Pure_g_utf8_find_prev_char(char const* arg0, char const* arg1)
{
  return g_utf8_find_prev_char(arg0, arg1);
}

long Pure_g_utf8_strlen(char const* arg0, long arg1)
{
  return g_utf8_strlen(arg0, arg1);
}

char* Pure_g_utf8_strncpy(char* arg0, char const* arg1, unsigned long arg2)
{
  return g_utf8_strncpy(arg0, arg1, arg2);
}

char* Pure_g_utf8_strchr(char const* arg0, long arg1, unsigned int arg2)
{
  return g_utf8_strchr(arg0, arg1, arg2);
}

char* Pure_g_utf8_strrchr(char const* arg0, long arg1, unsigned int arg2)
{
  return g_utf8_strrchr(arg0, arg1, arg2);
}

char* Pure_g_utf8_strreverse(char const* arg0, long arg1)
{
  return g_utf8_strreverse(arg0, arg1);
}

unsigned short* Pure_g_utf8_to_utf16(char const* arg0, long arg1, long* arg2, long* arg3, GError** arg4)
{
  return g_utf8_to_utf16(arg0, arg1, arg2, arg3, arg4);
}

unsigned int* Pure_g_utf8_to_ucs4(char const* arg0, long arg1, long* arg2, long* arg3, GError** arg4)
{
  return g_utf8_to_ucs4(arg0, arg1, arg2, arg3, arg4);
}

unsigned int* Pure_g_utf8_to_ucs4_fast(char const* arg0, long arg1, long* arg2)
{
  return g_utf8_to_ucs4_fast(arg0, arg1, arg2);
}

unsigned int* Pure_g_utf16_to_ucs4(unsigned short const* arg0, long arg1, long* arg2, long* arg3, GError** arg4)
{
  return g_utf16_to_ucs4(arg0, arg1, arg2, arg3, arg4);
}

char* Pure_g_utf16_to_utf8(unsigned short const* arg0, long arg1, long* arg2, long* arg3, GError** arg4)
{
  return g_utf16_to_utf8(arg0, arg1, arg2, arg3, arg4);
}

unsigned short* Pure_g_ucs4_to_utf16(unsigned int const* arg0, long arg1, long* arg2, long* arg3, GError** arg4)
{
  return g_ucs4_to_utf16(arg0, arg1, arg2, arg3, arg4);
}

char* Pure_g_ucs4_to_utf8(unsigned int const* arg0, long arg1, long* arg2, long* arg3, GError** arg4)
{
  return g_ucs4_to_utf8(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_unichar_to_utf8(unsigned int arg0, char* arg1)
{
  return g_unichar_to_utf8(arg0, arg1);
}

int Pure_g_utf8_validate(char const* arg0, long arg1, char const** arg2)
{
  return g_utf8_validate(arg0, arg1, arg2);
}

int Pure_g_unichar_validate(unsigned int arg0)
{
  return g_unichar_validate(arg0);
}

char* Pure_g_utf8_strup(char const* arg0, long arg1)
{
  return g_utf8_strup(arg0, arg1);
}

char* Pure_g_utf8_strdown(char const* arg0, long arg1)
{
  return g_utf8_strdown(arg0, arg1);
}

char* Pure_g_utf8_casefold(char const* arg0, long arg1)
{
  return g_utf8_casefold(arg0, arg1);
}

char* Pure_g_utf8_normalize(char const* arg0, long arg1, unsigned int arg2)
{
  return g_utf8_normalize(arg0, arg1, arg2);
}

int Pure_g_utf8_collate(char const* arg0, char const* arg1)
{
  return g_utf8_collate(arg0, arg1);
}

char* Pure_g_utf8_collate_key(char const* arg0, long arg1)
{
  return g_utf8_collate_key(arg0, arg1);
}

char* Pure_g_utf8_collate_key_for_filename(char const* arg0, long arg1)
{
  return g_utf8_collate_key_for_filename(arg0, arg1);
}

int Pure_g_unichar_get_mirror_char(unsigned int arg0, unsigned int* arg1)
{
  return g_unichar_get_mirror_char(arg0, arg1);
}

int Pure_g_unichar_get_script(unsigned int arg0)
{
  return g_unichar_get_script(arg0);
}

GStringChunk* Pure_g_string_chunk_new(unsigned long arg0)
{
  return g_string_chunk_new(arg0);
}

void Pure_g_string_chunk_free(GStringChunk* arg0)
{
  return g_string_chunk_free(arg0);
}

void Pure_g_string_chunk_clear(GStringChunk* arg0)
{
  return g_string_chunk_clear(arg0);
}

char* Pure_g_string_chunk_insert(GStringChunk* arg0, char const* arg1)
{
  return g_string_chunk_insert(arg0, arg1);
}

char* Pure_g_string_chunk_insert_len(GStringChunk* arg0, char const* arg1, long arg2)
{
  return g_string_chunk_insert_len(arg0, arg1, arg2);
}

char* Pure_g_string_chunk_insert_const(GStringChunk* arg0, char const* arg1)
{
  return g_string_chunk_insert_const(arg0, arg1);
}

GString* Pure_g_string_new(char const* arg0)
{
  return g_string_new(arg0);
}

GString* Pure_g_string_new_len(char const* arg0, long arg1)
{
  return g_string_new_len(arg0, arg1);
}

GString* Pure_g_string_sized_new(unsigned long arg0)
{
  return g_string_sized_new(arg0);
}

char* Pure_g_string_free(GString* arg0, int arg1)
{
  return g_string_free(arg0, arg1);
}

int Pure_g_string_equal(GString const* arg0, GString const* arg1)
{
  return g_string_equal(arg0, arg1);
}

unsigned int Pure_g_string_hash(GString const* arg0)
{
  return g_string_hash(arg0);
}

GString* Pure_g_string_assign(GString* arg0, char const* arg1)
{
  return g_string_assign(arg0, arg1);
}

GString* Pure_g_string_truncate(GString* arg0, unsigned long arg1)
{
  return g_string_truncate(arg0, arg1);
}

GString* Pure_g_string_set_size(GString* arg0, unsigned long arg1)
{
  return g_string_set_size(arg0, arg1);
}

GString* Pure_g_string_insert_len(GString* arg0, long arg1, char const* arg2, long arg3)
{
  return g_string_insert_len(arg0, arg1, arg2, arg3);
}

GString* Pure_g_string_append(GString* arg0, char const* arg1)
{
  return g_string_append(arg0, arg1);
}

GString* Pure_g_string_append_len(GString* arg0, char const* arg1, long arg2)
{
  return g_string_append_len(arg0, arg1, arg2);
}

GString* Pure_g_string_append_c(GString* arg0, char arg1)
{
  return g_string_append_c(arg0, arg1);
}

GString* Pure_g_string_append_unichar(GString* arg0, unsigned int arg1)
{
  return g_string_append_unichar(arg0, arg1);
}

GString* Pure_g_string_prepend(GString* arg0, char const* arg1)
{
  return g_string_prepend(arg0, arg1);
}

GString* Pure_g_string_prepend_c(GString* arg0, char arg1)
{
  return g_string_prepend_c(arg0, arg1);
}

GString* Pure_g_string_prepend_unichar(GString* arg0, unsigned int arg1)
{
  return g_string_prepend_unichar(arg0, arg1);
}

GString* Pure_g_string_prepend_len(GString* arg0, char const* arg1, long arg2)
{
  return g_string_prepend_len(arg0, arg1, arg2);
}

GString* Pure_g_string_insert(GString* arg0, long arg1, char const* arg2)
{
  return g_string_insert(arg0, arg1, arg2);
}

GString* Pure_g_string_insert_c(GString* arg0, long arg1, char arg2)
{
  return g_string_insert_c(arg0, arg1, arg2);
}

GString* Pure_g_string_insert_unichar(GString* arg0, long arg1, unsigned int arg2)
{
  return g_string_insert_unichar(arg0, arg1, arg2);
}

GString* Pure_g_string_overwrite(GString* arg0, unsigned long arg1, char const* arg2)
{
  return g_string_overwrite(arg0, arg1, arg2);
}

GString* Pure_g_string_overwrite_len(GString* arg0, unsigned long arg1, char const* arg2, long arg3)
{
  return g_string_overwrite_len(arg0, arg1, arg2, arg3);
}

GString* Pure_g_string_erase(GString* arg0, long arg1, long arg2)
{
  return g_string_erase(arg0, arg1, arg2);
}

GString* Pure_g_string_ascii_down(GString* arg0)
{
  return g_string_ascii_down(arg0);
}

GString* Pure_g_string_ascii_up(GString* arg0)
{
  return g_string_ascii_up(arg0);
}

void Pure_g_string_vprintf(GString* arg0, char const* arg1, void* arg2)
{
  return g_string_vprintf(arg0, arg1, arg2);
}

void Pure_g_string_printf(GString* arg0, char const* arg1)
{
  return g_string_printf(arg0, arg1);
}

void Pure_g_string_append_vprintf(GString* arg0, char const* arg1, void* arg2)
{
  return g_string_append_vprintf(arg0, arg1, arg2);
}

void Pure_g_string_append_printf(GString* arg0, char const* arg1)
{
  return g_string_append_printf(arg0, arg1);
}

GString* Pure_g_string_append_uri_escaped(GString* arg0, char const* arg1, char const* arg2, int arg3)
{
  return g_string_append_uri_escaped(arg0, arg1, arg2, arg3);
}

GString* Pure_g_string_down(GString* arg0)
{
  return g_string_down(arg0);
}

GString* Pure_g_string_up(GString* arg0)
{
  return g_string_up(arg0);
}

void Pure_g_io_channel_init(GIOChannel* arg0)
{
  return g_io_channel_init(arg0);
}

GIOChannel* Pure_g_io_channel_ref(GIOChannel* arg0)
{
  return g_io_channel_ref(arg0);
}

void Pure_g_io_channel_unref(GIOChannel* arg0)
{
  return g_io_channel_unref(arg0);
}

unsigned int Pure_g_io_channel_read(GIOChannel* arg0, char* arg1, unsigned long arg2, unsigned long* arg3)
{
  return g_io_channel_read(arg0, arg1, arg2, arg3);
}

unsigned int Pure_g_io_channel_write(GIOChannel* arg0, char const* arg1, unsigned long arg2, unsigned long* arg3)
{
  return g_io_channel_write(arg0, arg1, arg2, arg3);
}

unsigned int Pure_g_io_channel_seek(GIOChannel* arg0, long arg1, unsigned int arg2)
{
  return g_io_channel_seek(arg0, arg1, arg2);
}

void Pure_g_io_channel_close(GIOChannel* arg0)
{
  return g_io_channel_close(arg0);
}

unsigned int Pure_g_io_channel_shutdown(GIOChannel* arg0, int arg1, GError** arg2)
{
  return g_io_channel_shutdown(arg0, arg1, arg2);
}

unsigned int Pure_g_io_add_watch_full(GIOChannel* arg0, int arg1, unsigned int arg2, void* arg3, void* arg4, void* arg5)
{
  return g_io_add_watch_full(arg0, arg1, arg2, arg3, arg4, arg5);
}

GSource* Pure_g_io_create_watch(GIOChannel* arg0, unsigned int arg1)
{
  return g_io_create_watch(arg0, arg1);
}

unsigned int Pure_g_io_add_watch(GIOChannel* arg0, unsigned int arg1, void* arg2, void* arg3)
{
  return g_io_add_watch(arg0, arg1, arg2, arg3);
}

void Pure_g_io_channel_set_buffer_size(GIOChannel* arg0, unsigned long arg1)
{
  return g_io_channel_set_buffer_size(arg0, arg1);
}

unsigned long Pure_g_io_channel_get_buffer_size(GIOChannel* arg0)
{
  return g_io_channel_get_buffer_size(arg0);
}

unsigned int Pure_g_io_channel_get_buffer_condition(GIOChannel* arg0)
{
  return g_io_channel_get_buffer_condition(arg0);
}

unsigned int Pure_g_io_channel_set_flags(GIOChannel* arg0, unsigned int arg1, GError** arg2)
{
  return g_io_channel_set_flags(arg0, arg1, arg2);
}

unsigned int Pure_g_io_channel_get_flags(GIOChannel* arg0)
{
  return g_io_channel_get_flags(arg0);
}

void Pure_g_io_channel_set_line_term(GIOChannel* arg0, char const* arg1, int arg2)
{
  return g_io_channel_set_line_term(arg0, arg1, arg2);
}

char const* Pure_g_io_channel_get_line_term(GIOChannel* arg0, int* arg1)
{
  return g_io_channel_get_line_term(arg0, arg1);
}

void Pure_g_io_channel_set_buffered(GIOChannel* arg0, int arg1)
{
  return g_io_channel_set_buffered(arg0, arg1);
}

int Pure_g_io_channel_get_buffered(GIOChannel* arg0)
{
  return g_io_channel_get_buffered(arg0);
}

unsigned int Pure_g_io_channel_set_encoding(GIOChannel* arg0, char const* arg1, GError** arg2)
{
  return g_io_channel_set_encoding(arg0, arg1, arg2);
}

char const* Pure_g_io_channel_get_encoding(GIOChannel* arg0)
{
  return g_io_channel_get_encoding(arg0);
}

void Pure_g_io_channel_set_close_on_unref(GIOChannel* arg0, int arg1)
{
  return g_io_channel_set_close_on_unref(arg0, arg1);
}

int Pure_g_io_channel_get_close_on_unref(GIOChannel* arg0)
{
  return g_io_channel_get_close_on_unref(arg0);
}

unsigned int Pure_g_io_channel_flush(GIOChannel* arg0, GError** arg1)
{
  return g_io_channel_flush(arg0, arg1);
}

unsigned int Pure_g_io_channel_read_line(GIOChannel* arg0, char** arg1, unsigned long* arg2, unsigned long* arg3, GError** arg4)
{
  return g_io_channel_read_line(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_g_io_channel_read_line_string(GIOChannel* arg0, GString* arg1, unsigned long* arg2, GError** arg3)
{
  return g_io_channel_read_line_string(arg0, arg1, arg2, arg3);
}

unsigned int Pure_g_io_channel_read_to_end(GIOChannel* arg0, char** arg1, unsigned long* arg2, GError** arg3)
{
  return g_io_channel_read_to_end(arg0, arg1, arg2, arg3);
}

unsigned int Pure_g_io_channel_read_chars(GIOChannel* arg0, char* arg1, unsigned long arg2, unsigned long* arg3, GError** arg4)
{
  return g_io_channel_read_chars(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_g_io_channel_read_unichar(GIOChannel* arg0, unsigned int* arg1, GError** arg2)
{
  return g_io_channel_read_unichar(arg0, arg1, arg2);
}

unsigned int Pure_g_io_channel_write_chars(GIOChannel* arg0, char const* arg1, long arg2, unsigned long* arg3, GError** arg4)
{
  return g_io_channel_write_chars(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_g_io_channel_write_unichar(GIOChannel* arg0, unsigned int arg1, GError** arg2)
{
  return g_io_channel_write_unichar(arg0, arg1, arg2);
}

unsigned int Pure_g_io_channel_seek_position(GIOChannel* arg0, long arg1, unsigned int arg2, GError** arg3)
{
  return g_io_channel_seek_position(arg0, arg1, arg2, arg3);
}

GIOChannel* Pure_g_io_channel_new_file(char const* arg0, char const* arg1, GError** arg2)
{
  return g_io_channel_new_file(arg0, arg1, arg2);
}

unsigned int Pure_g_io_channel_error_quark()
{
  return g_io_channel_error_quark();
}

unsigned int Pure_g_io_channel_error_from_errno(int arg0)
{
  return g_io_channel_error_from_errno(arg0);
}

GIOChannel* Pure_g_io_channel_unix_new(int arg0)
{
  return g_io_channel_unix_new(arg0);
}

int Pure_g_io_channel_unix_get_fd(GIOChannel* arg0)
{
  return g_io_channel_unix_get_fd(arg0);
}

unsigned int Pure_g_key_file_error_quark()
{
  return g_key_file_error_quark();
}

GKeyFile* Pure_g_key_file_new()
{
  return g_key_file_new();
}

void Pure_g_key_file_free(GKeyFile* arg0)
{
  return g_key_file_free(arg0);
}

void Pure_g_key_file_set_list_separator(GKeyFile* arg0, char arg1)
{
  return g_key_file_set_list_separator(arg0, arg1);
}

int Pure_g_key_file_load_from_file(GKeyFile* arg0, char const* arg1, unsigned int arg2, GError** arg3)
{
  return g_key_file_load_from_file(arg0, arg1, arg2, arg3);
}

int Pure_g_key_file_load_from_data(GKeyFile* arg0, char const* arg1, unsigned long arg2, unsigned int arg3, GError** arg4)
{
  return g_key_file_load_from_data(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_key_file_load_from_dirs(GKeyFile* arg0, char const* arg1, char const** arg2, char** arg3, unsigned int arg4, GError** arg5)
{
  return g_key_file_load_from_dirs(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_g_key_file_load_from_data_dirs(GKeyFile* arg0, char const* arg1, char** arg2, unsigned int arg3, GError** arg4)
{
  return g_key_file_load_from_data_dirs(arg0, arg1, arg2, arg3, arg4);
}

char* Pure_g_key_file_to_data(GKeyFile* arg0, unsigned long* arg1, GError** arg2)
{
  return g_key_file_to_data(arg0, arg1, arg2);
}

char* Pure_g_key_file_get_start_group(GKeyFile* arg0)
{
  return g_key_file_get_start_group(arg0);
}

char** Pure_g_key_file_get_groups(GKeyFile* arg0, unsigned long* arg1)
{
  return g_key_file_get_groups(arg0, arg1);
}

char** Pure_g_key_file_get_keys(GKeyFile* arg0, char const* arg1, unsigned long* arg2, GError** arg3)
{
  return g_key_file_get_keys(arg0, arg1, arg2, arg3);
}

int Pure_g_key_file_has_group(GKeyFile* arg0, char const* arg1)
{
  return g_key_file_has_group(arg0, arg1);
}

int Pure_g_key_file_has_key(GKeyFile* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return g_key_file_has_key(arg0, arg1, arg2, arg3);
}

char* Pure_g_key_file_get_value(GKeyFile* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return g_key_file_get_value(arg0, arg1, arg2, arg3);
}

void Pure_g_key_file_set_value(GKeyFile* arg0, char const* arg1, char const* arg2, char const* arg3)
{
  return g_key_file_set_value(arg0, arg1, arg2, arg3);
}

char* Pure_g_key_file_get_string(GKeyFile* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return g_key_file_get_string(arg0, arg1, arg2, arg3);
}

void Pure_g_key_file_set_string(GKeyFile* arg0, char const* arg1, char const* arg2, char const* arg3)
{
  return g_key_file_set_string(arg0, arg1, arg2, arg3);
}

char* Pure_g_key_file_get_locale_string(GKeyFile* arg0, char const* arg1, char const* arg2, char const* arg3, GError** arg4)
{
  return g_key_file_get_locale_string(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_key_file_set_locale_string(GKeyFile* arg0, char const* arg1, char const* arg2, char const* arg3, char const* arg4)
{
  return g_key_file_set_locale_string(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_key_file_get_boolean(GKeyFile* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return g_key_file_get_boolean(arg0, arg1, arg2, arg3);
}

void Pure_g_key_file_set_boolean(GKeyFile* arg0, char const* arg1, char const* arg2, int arg3)
{
  return g_key_file_set_boolean(arg0, arg1, arg2, arg3);
}

int Pure_g_key_file_get_integer(GKeyFile* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return g_key_file_get_integer(arg0, arg1, arg2, arg3);
}

void Pure_g_key_file_set_integer(GKeyFile* arg0, char const* arg1, char const* arg2, int arg3)
{
  return g_key_file_set_integer(arg0, arg1, arg2, arg3);
}

double Pure_g_key_file_get_double(GKeyFile* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return g_key_file_get_double(arg0, arg1, arg2, arg3);
}

void Pure_g_key_file_set_double(GKeyFile* arg0, char const* arg1, char const* arg2, double arg3)
{
  return g_key_file_set_double(arg0, arg1, arg2, arg3);
}

char** Pure_g_key_file_get_string_list(GKeyFile* arg0, char const* arg1, char const* arg2, unsigned long* arg3, GError** arg4)
{
  return g_key_file_get_string_list(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_key_file_set_string_list(GKeyFile* arg0, char const* arg1, char const* arg2, char const* const* arg3, unsigned long arg4)
{
  return g_key_file_set_string_list(arg0, arg1, arg2, arg3, arg4);
}

char** Pure_g_key_file_get_locale_string_list(GKeyFile* arg0, char const* arg1, char const* arg2, char const* arg3, unsigned long* arg4, GError** arg5)
{
  return g_key_file_get_locale_string_list(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_key_file_set_locale_string_list(GKeyFile* arg0, char const* arg1, char const* arg2, char const* arg3, char const* const* arg4, unsigned long arg5)
{
  return g_key_file_set_locale_string_list(arg0, arg1, arg2, arg3, arg4, arg5);
}

int* Pure_g_key_file_get_boolean_list(GKeyFile* arg0, char const* arg1, char const* arg2, unsigned long* arg3, GError** arg4)
{
  return g_key_file_get_boolean_list(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_key_file_set_boolean_list(GKeyFile* arg0, char const* arg1, char const* arg2, int* arg3, unsigned long arg4)
{
  return g_key_file_set_boolean_list(arg0, arg1, arg2, arg3, arg4);
}

int* Pure_g_key_file_get_integer_list(GKeyFile* arg0, char const* arg1, char const* arg2, unsigned long* arg3, GError** arg4)
{
  return g_key_file_get_integer_list(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_key_file_set_double_list(GKeyFile* arg0, char const* arg1, char const* arg2, double* arg3, unsigned long arg4)
{
  return g_key_file_set_double_list(arg0, arg1, arg2, arg3, arg4);
}

double* Pure_g_key_file_get_double_list(GKeyFile* arg0, char const* arg1, char const* arg2, unsigned long* arg3, GError** arg4)
{
  return g_key_file_get_double_list(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_key_file_set_integer_list(GKeyFile* arg0, char const* arg1, char const* arg2, int* arg3, unsigned long arg4)
{
  return g_key_file_set_integer_list(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_key_file_set_comment(GKeyFile* arg0, char const* arg1, char const* arg2, char const* arg3, GError** arg4)
{
  return g_key_file_set_comment(arg0, arg1, arg2, arg3, arg4);
}

char* Pure_g_key_file_get_comment(GKeyFile* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return g_key_file_get_comment(arg0, arg1, arg2, arg3);
}

int Pure_g_key_file_remove_comment(GKeyFile* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return g_key_file_remove_comment(arg0, arg1, arg2, arg3);
}

int Pure_g_key_file_remove_key(GKeyFile* arg0, char const* arg1, char const* arg2, GError** arg3)
{
  return g_key_file_remove_key(arg0, arg1, arg2, arg3);
}

int Pure_g_key_file_remove_group(GKeyFile* arg0, char const* arg1, GError** arg2)
{
  return g_key_file_remove_group(arg0, arg1, arg2);
}

GMappedFile* Pure_g_mapped_file_new(char const* arg0, int arg1, GError** arg2)
{
  return g_mapped_file_new(arg0, arg1, arg2);
}

unsigned long Pure_g_mapped_file_get_length(GMappedFile* arg0)
{
  return g_mapped_file_get_length(arg0);
}

char* Pure_g_mapped_file_get_contents(GMappedFile* arg0)
{
  return g_mapped_file_get_contents(arg0);
}

void Pure_g_mapped_file_free(GMappedFile* arg0)
{
  return g_mapped_file_free(arg0);
}

unsigned int Pure_g_markup_error_quark()
{
  return g_markup_error_quark();
}

GMarkupParseContext* Pure_g_markup_parse_context_new(GMarkupParser const* arg0, unsigned int arg1, void* arg2, void* arg3)
{
  return g_markup_parse_context_new(arg0, arg1, arg2, arg3);
}

void Pure_g_markup_parse_context_free(GMarkupParseContext* arg0)
{
  return g_markup_parse_context_free(arg0);
}

int Pure_g_markup_parse_context_parse(GMarkupParseContext* arg0, char const* arg1, long arg2, GError** arg3)
{
  return g_markup_parse_context_parse(arg0, arg1, arg2, arg3);
}

void Pure_g_markup_parse_context_push(GMarkupParseContext* arg0, GMarkupParser* arg1, void* arg2)
{
  return g_markup_parse_context_push(arg0, arg1, arg2);
}

void* Pure_g_markup_parse_context_pop(GMarkupParseContext* arg0)
{
  return g_markup_parse_context_pop(arg0);
}

int Pure_g_markup_parse_context_end_parse(GMarkupParseContext* arg0, GError** arg1)
{
  return g_markup_parse_context_end_parse(arg0, arg1);
}

char const* Pure_g_markup_parse_context_get_element(GMarkupParseContext* arg0)
{
  return g_markup_parse_context_get_element(arg0);
}

GSList const* Pure_g_markup_parse_context_get_element_stack(GMarkupParseContext* arg0)
{
  return g_markup_parse_context_get_element_stack(arg0);
}

void Pure_g_markup_parse_context_get_position(GMarkupParseContext* arg0, int* arg1, int* arg2)
{
  return g_markup_parse_context_get_position(arg0, arg1, arg2);
}

void* Pure_g_markup_parse_context_get_user_data(GMarkupParseContext* arg0)
{
  return g_markup_parse_context_get_user_data(arg0);
}

char* Pure_g_markup_escape_text(char const* arg0, long arg1)
{
  return g_markup_escape_text(arg0, arg1);
}

char* Pure_g_markup_printf_escaped(char const* arg0)
{
  return g_markup_printf_escaped(arg0);
}

char* Pure_g_markup_vprintf_escaped(char const* arg0, void* arg1)
{
  return g_markup_vprintf_escaped(arg0, arg1);
}

int Pure_g_markup_collect_attributes(char const* arg0, char const** arg1, char const** arg2, GError** arg3, unsigned int arg4, char const* arg5)
{
  return g_markup_collect_attributes(arg0, arg1, arg2, arg3, arg4, arg5);
}

unsigned long Pure_g_printf_string_upper_bound(char const* arg0, void* arg1)
{
  return g_printf_string_upper_bound(arg0, arg1);
}

unsigned int Pure_g_log_set_handler(char const* arg0, int arg1, void* arg2, void* arg3)
{
  return g_log_set_handler(arg0, arg1, arg2, arg3);
}

void Pure_g_log_remove_handler(char const* arg0, unsigned int arg1)
{
  return g_log_remove_handler(arg0, arg1);
}

void Pure_g_log_default_handler(char const* arg0, int arg1, char const* arg2, void* arg3)
{
  return g_log_default_handler(arg0, arg1, arg2, arg3);
}

void* Pure_g_log_set_default_handler(void* arg0, void* arg1)
{
  return g_log_set_default_handler(arg0, arg1);
}

void Pure_g_log(char const* arg0, int arg1, char const* arg2)
{
  return g_log(arg0, arg1, arg2);
}

void Pure_g_logv(char const* arg0, int arg1, char const* arg2, void* arg3)
{
  return g_logv(arg0, arg1, arg2, arg3);
}

int Pure_g_log_set_fatal_mask(char const* arg0, int arg1)
{
  return g_log_set_fatal_mask(arg0, arg1);
}

int Pure_g_log_set_always_fatal(int arg0)
{
  return g_log_set_always_fatal(arg0);
}

void Pure_g_return_if_fail_warning(char const* arg0, char const* arg1, char const* arg2)
{
  return g_return_if_fail_warning(arg0, arg1, arg2);
}

void Pure_g_warn_message(char const* arg0, char const* arg1, int arg2, char const* arg3, char const* arg4)
{
  return g_warn_message(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_assert_warning(char const* arg0, char const* arg1, int const arg2, char const* arg3, char const* arg4)
{
  return g_assert_warning(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_print(char const* arg0)
{
  return g_print(arg0);
}

void* Pure_g_set_print_handler(void* arg0)
{
  return g_set_print_handler(arg0);
}

void Pure_g_printerr(char const* arg0)
{
  return g_printerr(arg0);
}

void* Pure_g_set_printerr_handler(void* arg0)
{
  return g_set_printerr_handler(arg0);
}

GNode* Pure_g_node_new(void* arg0)
{
  return g_node_new(arg0);
}

void Pure_g_node_destroy(GNode* arg0)
{
  return g_node_destroy(arg0);
}

void Pure_g_node_unlink(GNode* arg0)
{
  return g_node_unlink(arg0);
}

GNode* Pure_g_node_copy_deep(GNode* arg0, void* arg1, void* arg2)
{
  return g_node_copy_deep(arg0, arg1, arg2);
}

GNode* Pure_g_node_copy(GNode* arg0)
{
  return g_node_copy(arg0);
}

GNode* Pure_g_node_insert(GNode* arg0, int arg1, GNode* arg2)
{
  return g_node_insert(arg0, arg1, arg2);
}

GNode* Pure_g_node_insert_before(GNode* arg0, GNode* arg1, GNode* arg2)
{
  return g_node_insert_before(arg0, arg1, arg2);
}

GNode* Pure_g_node_insert_after(GNode* arg0, GNode* arg1, GNode* arg2)
{
  return g_node_insert_after(arg0, arg1, arg2);
}

GNode* Pure_g_node_prepend(GNode* arg0, GNode* arg1)
{
  return g_node_prepend(arg0, arg1);
}

unsigned int Pure_g_node_n_nodes(GNode* arg0, unsigned int arg1)
{
  return g_node_n_nodes(arg0, arg1);
}

GNode* Pure_g_node_get_root(GNode* arg0)
{
  return g_node_get_root(arg0);
}

int Pure_g_node_is_ancestor(GNode* arg0, GNode* arg1)
{
  return g_node_is_ancestor(arg0, arg1);
}

unsigned int Pure_g_node_depth(GNode* arg0)
{
  return g_node_depth(arg0);
}

GNode* Pure_g_node_find(GNode* arg0, unsigned int arg1, unsigned int arg2, void* arg3)
{
  return g_node_find(arg0, arg1, arg2, arg3);
}

void Pure_g_node_traverse(GNode* arg0, unsigned int arg1, unsigned int arg2, int arg3, void* arg4, void* arg5)
{
  return g_node_traverse(arg0, arg1, arg2, arg3, arg4, arg5);
}

unsigned int Pure_g_node_max_height(GNode* arg0)
{
  return g_node_max_height(arg0);
}

void Pure_g_node_children_foreach(GNode* arg0, unsigned int arg1, void* arg2, void* arg3)
{
  return g_node_children_foreach(arg0, arg1, arg2, arg3);
}

void Pure_g_node_reverse_children(GNode* arg0)
{
  return g_node_reverse_children(arg0);
}

unsigned int Pure_g_node_n_children(GNode* arg0)
{
  return g_node_n_children(arg0);
}

GNode* Pure_g_node_nth_child(GNode* arg0, unsigned int arg1)
{
  return g_node_nth_child(arg0, arg1);
}

GNode* Pure_g_node_last_child(GNode* arg0)
{
  return g_node_last_child(arg0);
}

GNode* Pure_g_node_find_child(GNode* arg0, unsigned int arg1, void* arg2)
{
  return g_node_find_child(arg0, arg1, arg2);
}

int Pure_g_node_child_position(GNode* arg0, GNode* arg1)
{
  return g_node_child_position(arg0, arg1);
}

int Pure_g_node_child_index(GNode* arg0, void* arg1)
{
  return g_node_child_index(arg0, arg1);
}

GNode* Pure_g_node_first_sibling(GNode* arg0)
{
  return g_node_first_sibling(arg0);
}

GNode* Pure_g_node_last_sibling(GNode* arg0)
{
  return g_node_last_sibling(arg0);
}

void Pure_g_node_push_allocator(void* arg0)
{
  return g_node_push_allocator(arg0);
}

void Pure_g_node_pop_allocator()
{
  return g_node_pop_allocator();
}

unsigned int Pure_g_option_error_quark()
{
  return g_option_error_quark();
}

GOptionContext* Pure_g_option_context_new(char const* arg0)
{
  return g_option_context_new(arg0);
}

void Pure_g_option_context_set_summary(GOptionContext* arg0, char const* arg1)
{
  return g_option_context_set_summary(arg0, arg1);
}

char const* Pure_g_option_context_get_summary(GOptionContext* arg0)
{
  return g_option_context_get_summary(arg0);
}

void Pure_g_option_context_set_description(GOptionContext* arg0, char const* arg1)
{
  return g_option_context_set_description(arg0, arg1);
}

char const* Pure_g_option_context_get_description(GOptionContext* arg0)
{
  return g_option_context_get_description(arg0);
}

void Pure_g_option_context_free(GOptionContext* arg0)
{
  return g_option_context_free(arg0);
}

void Pure_g_option_context_set_help_enabled(GOptionContext* arg0, int arg1)
{
  return g_option_context_set_help_enabled(arg0, arg1);
}

int Pure_g_option_context_get_help_enabled(GOptionContext* arg0)
{
  return g_option_context_get_help_enabled(arg0);
}

void Pure_g_option_context_set_ignore_unknown_options(GOptionContext* arg0, int arg1)
{
  return g_option_context_set_ignore_unknown_options(arg0, arg1);
}

int Pure_g_option_context_get_ignore_unknown_options(GOptionContext* arg0)
{
  return g_option_context_get_ignore_unknown_options(arg0);
}

void Pure_g_option_context_add_main_entries(GOptionContext* arg0, GOptionEntry const* arg1, char const* arg2)
{
  return g_option_context_add_main_entries(arg0, arg1, arg2);
}

int Pure_g_option_context_parse(GOptionContext* arg0, int* arg1, char*** arg2, GError** arg3)
{
  return g_option_context_parse(arg0, arg1, arg2, arg3);
}

void Pure_g_option_context_set_translate_func(GOptionContext* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_option_context_set_translate_func(arg0, arg1, arg2, arg3);
}

void Pure_g_option_context_set_translation_domain(GOptionContext* arg0, char const* arg1)
{
  return g_option_context_set_translation_domain(arg0, arg1);
}

void Pure_g_option_context_add_group(GOptionContext* arg0, GOptionGroup* arg1)
{
  return g_option_context_add_group(arg0, arg1);
}

void Pure_g_option_context_set_main_group(GOptionContext* arg0, GOptionGroup* arg1)
{
  return g_option_context_set_main_group(arg0, arg1);
}

GOptionGroup* Pure_g_option_context_get_main_group(GOptionContext* arg0)
{
  return g_option_context_get_main_group(arg0);
}

char* Pure_g_option_context_get_help(GOptionContext* arg0, int arg1, GOptionGroup* arg2)
{
  return g_option_context_get_help(arg0, arg1, arg2);
}

GOptionGroup* Pure_g_option_group_new(char const* arg0, char const* arg1, char const* arg2, void* arg3, void* arg4)
{
  return g_option_group_new(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_option_group_set_parse_hooks(GOptionGroup* arg0, void* arg1, void* arg2)
{
  return g_option_group_set_parse_hooks(arg0, arg1, arg2);
}

void Pure_g_option_group_set_error_hook(GOptionGroup* arg0, void* arg1)
{
  return g_option_group_set_error_hook(arg0, arg1);
}

void Pure_g_option_group_free(GOptionGroup* arg0)
{
  return g_option_group_free(arg0);
}

void Pure_g_option_group_add_entries(GOptionGroup* arg0, GOptionEntry const* arg1)
{
  return g_option_group_add_entries(arg0, arg1);
}

void Pure_g_option_group_set_translate_func(GOptionGroup* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_option_group_set_translate_func(arg0, arg1, arg2, arg3);
}

void Pure_g_option_group_set_translation_domain(GOptionGroup* arg0, char const* arg1)
{
  return g_option_group_set_translation_domain(arg0, arg1);
}

GPatternSpec* Pure_g_pattern_spec_new(char const* arg0)
{
  return g_pattern_spec_new(arg0);
}

void Pure_g_pattern_spec_free(GPatternSpec* arg0)
{
  return g_pattern_spec_free(arg0);
}

int Pure_g_pattern_spec_equal(GPatternSpec* arg0, GPatternSpec* arg1)
{
  return g_pattern_spec_equal(arg0, arg1);
}

int Pure_g_pattern_match(GPatternSpec* arg0, unsigned int arg1, char const* arg2, char const* arg3)
{
  return g_pattern_match(arg0, arg1, arg2, arg3);
}

int Pure_g_pattern_match_string(GPatternSpec* arg0, char const* arg1)
{
  return g_pattern_match_string(arg0, arg1);
}

int Pure_g_pattern_match_simple(char const* arg0, char const* arg1)
{
  return g_pattern_match_simple(arg0, arg1);
}

unsigned int Pure_g_spaced_primes_closest(unsigned int arg0)
{
  return g_spaced_primes_closest(arg0);
}

void Pure_g_qsort_with_data(void const* arg0, int arg1, unsigned long arg2, void* arg3, void* arg4)
{
  return g_qsort_with_data(arg0, arg1, arg2, arg3, arg4);
}

GQueue* Pure_g_queue_new()
{
  return g_queue_new();
}

void Pure_g_queue_free(GQueue* arg0)
{
  return g_queue_free(arg0);
}

void Pure_g_queue_init(GQueue* arg0)
{
  return g_queue_init(arg0);
}

void Pure_g_queue_clear(GQueue* arg0)
{
  return g_queue_clear(arg0);
}

int Pure_g_queue_is_empty(GQueue* arg0)
{
  return g_queue_is_empty(arg0);
}

unsigned int Pure_g_queue_get_length(GQueue* arg0)
{
  return g_queue_get_length(arg0);
}

void Pure_g_queue_reverse(GQueue* arg0)
{
  return g_queue_reverse(arg0);
}

GQueue* Pure_g_queue_copy(GQueue* arg0)
{
  return g_queue_copy(arg0);
}

void Pure_g_queue_foreach(GQueue* arg0, void* arg1, void* arg2)
{
  return g_queue_foreach(arg0, arg1, arg2);
}

GList* Pure_g_queue_find(GQueue* arg0, void const* arg1)
{
  return g_queue_find(arg0, arg1);
}

GList* Pure_g_queue_find_custom(GQueue* arg0, void const* arg1, void* arg2)
{
  return g_queue_find_custom(arg0, arg1, arg2);
}

void Pure_g_queue_sort(GQueue* arg0, void* arg1, void* arg2)
{
  return g_queue_sort(arg0, arg1, arg2);
}

void Pure_g_queue_push_head(GQueue* arg0, void* arg1)
{
  return g_queue_push_head(arg0, arg1);
}

void Pure_g_queue_push_tail(GQueue* arg0, void* arg1)
{
  return g_queue_push_tail(arg0, arg1);
}

void Pure_g_queue_push_nth(GQueue* arg0, void* arg1, int arg2)
{
  return g_queue_push_nth(arg0, arg1, arg2);
}

void* Pure_g_queue_pop_head(GQueue* arg0)
{
  return g_queue_pop_head(arg0);
}

void* Pure_g_queue_pop_tail(GQueue* arg0)
{
  return g_queue_pop_tail(arg0);
}

void* Pure_g_queue_pop_nth(GQueue* arg0, unsigned int arg1)
{
  return g_queue_pop_nth(arg0, arg1);
}

void* Pure_g_queue_peek_head(GQueue* arg0)
{
  return g_queue_peek_head(arg0);
}

void* Pure_g_queue_peek_tail(GQueue* arg0)
{
  return g_queue_peek_tail(arg0);
}

void* Pure_g_queue_peek_nth(GQueue* arg0, unsigned int arg1)
{
  return g_queue_peek_nth(arg0, arg1);
}

int Pure_g_queue_index(GQueue* arg0, void const* arg1)
{
  return g_queue_index(arg0, arg1);
}

void Pure_g_queue_remove(GQueue* arg0, void const* arg1)
{
  return g_queue_remove(arg0, arg1);
}

void Pure_g_queue_remove_all(GQueue* arg0, void const* arg1)
{
  return g_queue_remove_all(arg0, arg1);
}

void Pure_g_queue_insert_before(GQueue* arg0, GList* arg1, void* arg2)
{
  return g_queue_insert_before(arg0, arg1, arg2);
}

void Pure_g_queue_insert_after(GQueue* arg0, GList* arg1, void* arg2)
{
  return g_queue_insert_after(arg0, arg1, arg2);
}

void Pure_g_queue_insert_sorted(GQueue* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_queue_insert_sorted(arg0, arg1, arg2, arg3);
}

void Pure_g_queue_push_head_link(GQueue* arg0, GList* arg1)
{
  return g_queue_push_head_link(arg0, arg1);
}

void Pure_g_queue_push_tail_link(GQueue* arg0, GList* arg1)
{
  return g_queue_push_tail_link(arg0, arg1);
}

void Pure_g_queue_push_nth_link(GQueue* arg0, int arg1, GList* arg2)
{
  return g_queue_push_nth_link(arg0, arg1, arg2);
}

GList* Pure_g_queue_pop_head_link(GQueue* arg0)
{
  return g_queue_pop_head_link(arg0);
}

GList* Pure_g_queue_pop_tail_link(GQueue* arg0)
{
  return g_queue_pop_tail_link(arg0);
}

GList* Pure_g_queue_pop_nth_link(GQueue* arg0, unsigned int arg1)
{
  return g_queue_pop_nth_link(arg0, arg1);
}

GList* Pure_g_queue_peek_head_link(GQueue* arg0)
{
  return g_queue_peek_head_link(arg0);
}

GList* Pure_g_queue_peek_tail_link(GQueue* arg0)
{
  return g_queue_peek_tail_link(arg0);
}

GList* Pure_g_queue_peek_nth_link(GQueue* arg0, unsigned int arg1)
{
  return g_queue_peek_nth_link(arg0, arg1);
}

int Pure_g_queue_link_index(GQueue* arg0, GList* arg1)
{
  return g_queue_link_index(arg0, arg1);
}

void Pure_g_queue_unlink(GQueue* arg0, GList* arg1)
{
  return g_queue_unlink(arg0, arg1);
}

void Pure_g_queue_delete_link(GQueue* arg0, GList* arg1)
{
  return g_queue_delete_link(arg0, arg1);
}

GRand* Pure_g_rand_new_with_seed(unsigned int arg0)
{
  return g_rand_new_with_seed(arg0);
}

GRand* Pure_g_rand_new_with_seed_array(unsigned int const* arg0, unsigned int arg1)
{
  return g_rand_new_with_seed_array(arg0, arg1);
}

GRand* Pure_g_rand_new()
{
  return g_rand_new();
}

void Pure_g_rand_free(GRand* arg0)
{
  return g_rand_free(arg0);
}

GRand* Pure_g_rand_copy(GRand* arg0)
{
  return g_rand_copy(arg0);
}

void Pure_g_rand_set_seed(GRand* arg0, unsigned int arg1)
{
  return g_rand_set_seed(arg0, arg1);
}

void Pure_g_rand_set_seed_array(GRand* arg0, unsigned int const* arg1, unsigned int arg2)
{
  return g_rand_set_seed_array(arg0, arg1, arg2);
}

unsigned int Pure_g_rand_int(GRand* arg0)
{
  return g_rand_int(arg0);
}

int Pure_g_rand_int_range(GRand* arg0, int arg1, int arg2)
{
  return g_rand_int_range(arg0, arg1, arg2);
}

double Pure_g_rand_double(GRand* arg0)
{
  return g_rand_double(arg0);
}

double Pure_g_rand_double_range(GRand* arg0, double arg1, double arg2)
{
  return g_rand_double_range(arg0, arg1, arg2);
}

void Pure_g_random_set_seed(unsigned int arg0)
{
  return g_random_set_seed(arg0);
}

unsigned int Pure_g_random_int()
{
  return g_random_int();
}

int Pure_g_random_int_range(int arg0, int arg1)
{
  return g_random_int_range(arg0, arg1);
}

double Pure_g_random_double()
{
  return g_random_double();
}

double Pure_g_random_double_range(double arg0, double arg1)
{
  return g_random_double_range(arg0, arg1);
}

GRelation* Pure_g_relation_new(int arg0)
{
  return g_relation_new(arg0);
}

void Pure_g_relation_destroy(GRelation* arg0)
{
  return g_relation_destroy(arg0);
}

void Pure_g_relation_index(GRelation* arg0, int arg1, void* arg2, void* arg3)
{
  return g_relation_index(arg0, arg1, arg2, arg3);
}

void Pure_g_relation_insert(GRelation* arg0)
{
  return g_relation_insert(arg0);
}

int Pure_g_relation_delete(GRelation* arg0, void const* arg1, int arg2)
{
  return g_relation_delete(arg0, arg1, arg2);
}

GTuples* Pure_g_relation_select(GRelation* arg0, void const* arg1, int arg2)
{
  return g_relation_select(arg0, arg1, arg2);
}

int Pure_g_relation_count(GRelation* arg0, void const* arg1, int arg2)
{
  return g_relation_count(arg0, arg1, arg2);
}

int Pure_g_relation_exists(GRelation* arg0)
{
  return g_relation_exists(arg0);
}

void Pure_g_relation_print(GRelation* arg0)
{
  return g_relation_print(arg0);
}

void Pure_g_tuples_destroy(GTuples* arg0)
{
  return g_tuples_destroy(arg0);
}

void* Pure_g_tuples_index(GTuples* arg0, int arg1, int arg2)
{
  return g_tuples_index(arg0, arg1, arg2);
}

unsigned int Pure_g_regex_error_quark()
{
  return g_regex_error_quark();
}

GRegex* Pure_g_regex_new(char const* arg0, unsigned int arg1, unsigned int arg2, GError** arg3)
{
  return g_regex_new(arg0, arg1, arg2, arg3);
}

GRegex* Pure_g_regex_ref(GRegex* arg0)
{
  return g_regex_ref(arg0);
}

void Pure_g_regex_unref(GRegex* arg0)
{
  return g_regex_unref(arg0);
}

char const* Pure_g_regex_get_pattern(GRegex const* arg0)
{
  return g_regex_get_pattern(arg0);
}

int Pure_g_regex_get_max_backref(GRegex const* arg0)
{
  return g_regex_get_max_backref(arg0);
}

int Pure_g_regex_get_capture_count(GRegex const* arg0)
{
  return g_regex_get_capture_count(arg0);
}

int Pure_g_regex_get_string_number(GRegex const* arg0, char const* arg1)
{
  return g_regex_get_string_number(arg0, arg1);
}

char* Pure_g_regex_escape_string(char const* arg0, int arg1)
{
  return g_regex_escape_string(arg0, arg1);
}

int Pure_g_regex_match_simple(char const* arg0, char const* arg1, unsigned int arg2, unsigned int arg3)
{
  return g_regex_match_simple(arg0, arg1, arg2, arg3);
}

int Pure_g_regex_match(GRegex const* arg0, char const* arg1, unsigned int arg2, GMatchInfo** arg3)
{
  return g_regex_match(arg0, arg1, arg2, arg3);
}

int Pure_g_regex_match_full(GRegex const* arg0, char const* arg1, long arg2, int arg3, unsigned int arg4, GMatchInfo** arg5, GError** arg6)
{
  return g_regex_match_full(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

int Pure_g_regex_match_all(GRegex const* arg0, char const* arg1, unsigned int arg2, GMatchInfo** arg3)
{
  return g_regex_match_all(arg0, arg1, arg2, arg3);
}

int Pure_g_regex_match_all_full(GRegex const* arg0, char const* arg1, long arg2, int arg3, unsigned int arg4, GMatchInfo** arg5, GError** arg6)
{
  return g_regex_match_all_full(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

char** Pure_g_regex_split_simple(char const* arg0, char const* arg1, unsigned int arg2, unsigned int arg3)
{
  return g_regex_split_simple(arg0, arg1, arg2, arg3);
}

char** Pure_g_regex_split(GRegex const* arg0, char const* arg1, unsigned int arg2)
{
  return g_regex_split(arg0, arg1, arg2);
}

char** Pure_g_regex_split_full(GRegex const* arg0, char const* arg1, long arg2, int arg3, unsigned int arg4, int arg5, GError** arg6)
{
  return g_regex_split_full(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

char* Pure_g_regex_replace(GRegex const* arg0, char const* arg1, long arg2, int arg3, char const* arg4, unsigned int arg5, GError** arg6)
{
  return g_regex_replace(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

char* Pure_g_regex_replace_literal(GRegex const* arg0, char const* arg1, long arg2, int arg3, char const* arg4, unsigned int arg5, GError** arg6)
{
  return g_regex_replace_literal(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

char* Pure_g_regex_replace_eval(GRegex const* arg0, char const* arg1, long arg2, int arg3, unsigned int arg4, void* arg5, void* arg6, GError** arg7)
{
  return g_regex_replace_eval(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

int Pure_g_regex_check_replacement(char const* arg0, int* arg1, GError** arg2)
{
  return g_regex_check_replacement(arg0, arg1, arg2);
}

GRegex* Pure_g_match_info_get_regex(GMatchInfo const* arg0)
{
  return g_match_info_get_regex(arg0);
}

char const* Pure_g_match_info_get_string(GMatchInfo const* arg0)
{
  return g_match_info_get_string(arg0);
}

void Pure_g_match_info_free(GMatchInfo* arg0)
{
  return g_match_info_free(arg0);
}

int Pure_g_match_info_next(GMatchInfo* arg0, GError** arg1)
{
  return g_match_info_next(arg0, arg1);
}

int Pure_g_match_info_matches(GMatchInfo const* arg0)
{
  return g_match_info_matches(arg0);
}

int Pure_g_match_info_get_match_count(GMatchInfo const* arg0)
{
  return g_match_info_get_match_count(arg0);
}

int Pure_g_match_info_is_partial_match(GMatchInfo const* arg0)
{
  return g_match_info_is_partial_match(arg0);
}

char* Pure_g_match_info_expand_references(GMatchInfo const* arg0, char const* arg1, GError** arg2)
{
  return g_match_info_expand_references(arg0, arg1, arg2);
}

char* Pure_g_match_info_fetch(GMatchInfo const* arg0, int arg1)
{
  return g_match_info_fetch(arg0, arg1);
}

int Pure_g_match_info_fetch_pos(GMatchInfo const* arg0, int arg1, int* arg2, int* arg3)
{
  return g_match_info_fetch_pos(arg0, arg1, arg2, arg3);
}

char* Pure_g_match_info_fetch_named(GMatchInfo const* arg0, char const* arg1)
{
  return g_match_info_fetch_named(arg0, arg1);
}

int Pure_g_match_info_fetch_named_pos(GMatchInfo const* arg0, char const* arg1, int* arg2, int* arg3)
{
  return g_match_info_fetch_named_pos(arg0, arg1, arg2, arg3);
}

char** Pure_g_match_info_fetch_all(GMatchInfo const* arg0)
{
  return g_match_info_fetch_all(arg0);
}

GScanner* Pure_g_scanner_new(GScannerConfig const* arg0)
{
  return g_scanner_new(arg0);
}

void Pure_g_scanner_destroy(GScanner* arg0)
{
  return g_scanner_destroy(arg0);
}

void Pure_g_scanner_input_file(GScanner* arg0, int arg1)
{
  return g_scanner_input_file(arg0, arg1);
}

void Pure_g_scanner_sync_file_offset(GScanner* arg0)
{
  return g_scanner_sync_file_offset(arg0);
}

void Pure_g_scanner_input_text(GScanner* arg0, char const* arg1, unsigned int arg2)
{
  return g_scanner_input_text(arg0, arg1, arg2);
}

unsigned int Pure_g_scanner_get_next_token(GScanner* arg0)
{
  return g_scanner_get_next_token(arg0);
}

unsigned int Pure_g_scanner_peek_next_token(GScanner* arg0)
{
  return g_scanner_peek_next_token(arg0);
}

unsigned int Pure_g_scanner_cur_token(GScanner* arg0)
{
  return g_scanner_cur_token(arg0);
}

GTokenValue* Pure_g_scanner_cur_value(GScanner* arg0)
{
  static GTokenValue ret;
  ret = g_scanner_cur_value(arg0); return &ret;
}

unsigned int Pure_g_scanner_cur_line(GScanner* arg0)
{
  return g_scanner_cur_line(arg0);
}

unsigned int Pure_g_scanner_cur_position(GScanner* arg0)
{
  return g_scanner_cur_position(arg0);
}

int Pure_g_scanner_eof(GScanner* arg0)
{
  return g_scanner_eof(arg0);
}

unsigned int Pure_g_scanner_set_scope(GScanner* arg0, unsigned int arg1)
{
  return g_scanner_set_scope(arg0, arg1);
}

void Pure_g_scanner_scope_add_symbol(GScanner* arg0, unsigned int arg1, char const* arg2, void* arg3)
{
  return g_scanner_scope_add_symbol(arg0, arg1, arg2, arg3);
}

void Pure_g_scanner_scope_remove_symbol(GScanner* arg0, unsigned int arg1, char const* arg2)
{
  return g_scanner_scope_remove_symbol(arg0, arg1, arg2);
}

void* Pure_g_scanner_scope_lookup_symbol(GScanner* arg0, unsigned int arg1, char const* arg2)
{
  return g_scanner_scope_lookup_symbol(arg0, arg1, arg2);
}

void Pure_g_scanner_scope_foreach_symbol(GScanner* arg0, unsigned int arg1, void* arg2, void* arg3)
{
  return g_scanner_scope_foreach_symbol(arg0, arg1, arg2, arg3);
}

void* Pure_g_scanner_lookup_symbol(GScanner* arg0, char const* arg1)
{
  return g_scanner_lookup_symbol(arg0, arg1);
}

void Pure_g_scanner_unexp_token(GScanner* arg0, unsigned int arg1, char const* arg2, char const* arg3, char const* arg4, char const* arg5, int arg6)
{
  return g_scanner_unexp_token(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_g_scanner_error(GScanner* arg0, char const* arg1)
{
  return g_scanner_error(arg0, arg1);
}

void Pure_g_scanner_warn(GScanner* arg0, char const* arg1)
{
  return g_scanner_warn(arg0, arg1);
}

GSequence* Pure_g_sequence_new(void* arg0)
{
  return g_sequence_new(arg0);
}

void Pure_g_sequence_free(GSequence* arg0)
{
  return g_sequence_free(arg0);
}

int Pure_g_sequence_get_length(GSequence* arg0)
{
  return g_sequence_get_length(arg0);
}

void Pure_g_sequence_foreach(GSequence* arg0, void* arg1, void* arg2)
{
  return g_sequence_foreach(arg0, arg1, arg2);
}

void Pure_g_sequence_foreach_range(GSequenceIter* arg0, GSequenceIter* arg1, void* arg2, void* arg3)
{
  return g_sequence_foreach_range(arg0, arg1, arg2, arg3);
}

void Pure_g_sequence_sort(GSequence* arg0, void* arg1, void* arg2)
{
  return g_sequence_sort(arg0, arg1, arg2);
}

void Pure_g_sequence_sort_iter(GSequence* arg0, void* arg1, void* arg2)
{
  return g_sequence_sort_iter(arg0, arg1, arg2);
}

GSequenceIter* Pure_g_sequence_get_begin_iter(GSequence* arg0)
{
  return g_sequence_get_begin_iter(arg0);
}

GSequenceIter* Pure_g_sequence_get_end_iter(GSequence* arg0)
{
  return g_sequence_get_end_iter(arg0);
}

GSequenceIter* Pure_g_sequence_get_iter_at_pos(GSequence* arg0, int arg1)
{
  return g_sequence_get_iter_at_pos(arg0, arg1);
}

GSequenceIter* Pure_g_sequence_append(GSequence* arg0, void* arg1)
{
  return g_sequence_append(arg0, arg1);
}

GSequenceIter* Pure_g_sequence_prepend(GSequence* arg0, void* arg1)
{
  return g_sequence_prepend(arg0, arg1);
}

GSequenceIter* Pure_g_sequence_insert_before(GSequenceIter* arg0, void* arg1)
{
  return g_sequence_insert_before(arg0, arg1);
}

void Pure_g_sequence_move(GSequenceIter* arg0, GSequenceIter* arg1)
{
  return g_sequence_move(arg0, arg1);
}

void Pure_g_sequence_swap(GSequenceIter* arg0, GSequenceIter* arg1)
{
  return g_sequence_swap(arg0, arg1);
}

GSequenceIter* Pure_g_sequence_insert_sorted(GSequence* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_sequence_insert_sorted(arg0, arg1, arg2, arg3);
}

GSequenceIter* Pure_g_sequence_insert_sorted_iter(GSequence* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_sequence_insert_sorted_iter(arg0, arg1, arg2, arg3);
}

void Pure_g_sequence_sort_changed(GSequenceIter* arg0, void* arg1, void* arg2)
{
  return g_sequence_sort_changed(arg0, arg1, arg2);
}

void Pure_g_sequence_sort_changed_iter(GSequenceIter* arg0, void* arg1, void* arg2)
{
  return g_sequence_sort_changed_iter(arg0, arg1, arg2);
}

void Pure_g_sequence_remove(GSequenceIter* arg0)
{
  return g_sequence_remove(arg0);
}

void Pure_g_sequence_remove_range(GSequenceIter* arg0, GSequenceIter* arg1)
{
  return g_sequence_remove_range(arg0, arg1);
}

void Pure_g_sequence_move_range(GSequenceIter* arg0, GSequenceIter* arg1, GSequenceIter* arg2)
{
  return g_sequence_move_range(arg0, arg1, arg2);
}

GSequenceIter* Pure_g_sequence_search(GSequence* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_sequence_search(arg0, arg1, arg2, arg3);
}

GSequenceIter* Pure_g_sequence_search_iter(GSequence* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_sequence_search_iter(arg0, arg1, arg2, arg3);
}

void* Pure_g_sequence_get(GSequenceIter* arg0)
{
  return g_sequence_get(arg0);
}

void Pure_g_sequence_set(GSequenceIter* arg0, void* arg1)
{
  return g_sequence_set(arg0, arg1);
}

int Pure_g_sequence_iter_is_begin(GSequenceIter* arg0)
{
  return g_sequence_iter_is_begin(arg0);
}

int Pure_g_sequence_iter_is_end(GSequenceIter* arg0)
{
  return g_sequence_iter_is_end(arg0);
}

GSequenceIter* Pure_g_sequence_iter_next(GSequenceIter* arg0)
{
  return g_sequence_iter_next(arg0);
}

GSequenceIter* Pure_g_sequence_iter_prev(GSequenceIter* arg0)
{
  return g_sequence_iter_prev(arg0);
}

int Pure_g_sequence_iter_get_position(GSequenceIter* arg0)
{
  return g_sequence_iter_get_position(arg0);
}

GSequenceIter* Pure_g_sequence_iter_move(GSequenceIter* arg0, int arg1)
{
  return g_sequence_iter_move(arg0, arg1);
}

GSequence* Pure_g_sequence_iter_get_sequence(GSequenceIter* arg0)
{
  return g_sequence_iter_get_sequence(arg0);
}

int Pure_g_sequence_iter_compare(GSequenceIter* arg0, GSequenceIter* arg1)
{
  return g_sequence_iter_compare(arg0, arg1);
}

GSequenceIter* Pure_g_sequence_range_get_midpoint(GSequenceIter* arg0, GSequenceIter* arg1)
{
  return g_sequence_range_get_midpoint(arg0, arg1);
}

unsigned int Pure_g_shell_error_quark()
{
  return g_shell_error_quark();
}

char* Pure_g_shell_quote(char const* arg0)
{
  return g_shell_quote(arg0);
}

char* Pure_g_shell_unquote(char const* arg0, GError** arg1)
{
  return g_shell_unquote(arg0, arg1);
}

int Pure_g_shell_parse_argv(char const* arg0, int* arg1, char*** arg2, GError** arg3)
{
  return g_shell_parse_argv(arg0, arg1, arg2, arg3);
}

unsigned int Pure_g_spawn_error_quark()
{
  return g_spawn_error_quark();
}

int Pure_g_spawn_async(char const* arg0, char** arg1, char** arg2, unsigned int arg3, void* arg4, void* arg5, int* arg6, GError** arg7)
{
  return g_spawn_async(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

int Pure_g_spawn_async_with_pipes(char const* arg0, char** arg1, char** arg2, unsigned int arg3, void* arg4, void* arg5, int* arg6, int* arg7, int* arg8, int* arg9, GError** arg10)
{
  return g_spawn_async_with_pipes(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

int Pure_g_spawn_sync(char const* arg0, char** arg1, char** arg2, unsigned int arg3, void* arg4, void* arg5, char** arg6, char** arg7, int* arg8, GError** arg9)
{
  return g_spawn_sync(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

int Pure_g_spawn_command_line_sync(char const* arg0, char** arg1, char** arg2, int* arg3, GError** arg4)
{
  return g_spawn_command_line_sync(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_spawn_command_line_async(char const* arg0, GError** arg1)
{
  return g_spawn_command_line_async(arg0, arg1);
}

void Pure_g_spawn_close_pid(int arg0)
{
  return g_spawn_close_pid(arg0);
}

char Pure_g_ascii_tolower(char arg0)
{
  return g_ascii_tolower(arg0);
}

char Pure_g_ascii_toupper(char arg0)
{
  return g_ascii_toupper(arg0);
}

int Pure_g_ascii_digit_value(char arg0)
{
  return g_ascii_digit_value(arg0);
}

int Pure_g_ascii_xdigit_value(char arg0)
{
  return g_ascii_xdigit_value(arg0);
}

char* Pure_g_strdelimit(char* arg0, char const* arg1, char arg2)
{
  return g_strdelimit(arg0, arg1, arg2);
}

char* Pure_g_strcanon(char* arg0, char const* arg1, char arg2)
{
  return g_strcanon(arg0, arg1, arg2);
}

char const* Pure_g_strerror(int arg0)
{
  return g_strerror(arg0);
}

char const* Pure_g_strsignal(int arg0)
{
  return g_strsignal(arg0);
}

char* Pure_g_strreverse(char* arg0)
{
  return g_strreverse(arg0);
}

unsigned long Pure_g_strlcpy(char* arg0, char const* arg1, unsigned long arg2)
{
  return g_strlcpy(arg0, arg1, arg2);
}

unsigned long Pure_g_strlcat(char* arg0, char const* arg1, unsigned long arg2)
{
  return g_strlcat(arg0, arg1, arg2);
}

char* Pure_g_strstr_len(char const* arg0, long arg1, char const* arg2)
{
  return g_strstr_len(arg0, arg1, arg2);
}

char* Pure_g_strrstr(char const* arg0, char const* arg1)
{
  return g_strrstr(arg0, arg1);
}

char* Pure_g_strrstr_len(char const* arg0, long arg1, char const* arg2)
{
  return g_strrstr_len(arg0, arg1, arg2);
}

int Pure_g_str_has_suffix(char const* arg0, char const* arg1)
{
  return g_str_has_suffix(arg0, arg1);
}

int Pure_g_str_has_prefix(char const* arg0, char const* arg1)
{
  return g_str_has_prefix(arg0, arg1);
}

double Pure_g_strtod(char const* arg0, char** arg1)
{
  return g_strtod(arg0, arg1);
}

double Pure_g_ascii_strtod(char const* arg0, char** arg1)
{
  return g_ascii_strtod(arg0, arg1);
}

unsigned long Pure_g_ascii_strtoull(char const* arg0, char** arg1, unsigned int arg2)
{
  return g_ascii_strtoull(arg0, arg1, arg2);
}

long Pure_g_ascii_strtoll(char const* arg0, char** arg1, unsigned int arg2)
{
  return g_ascii_strtoll(arg0, arg1, arg2);
}

char* Pure_g_ascii_dtostr(char* arg0, int arg1, double arg2)
{
  return g_ascii_dtostr(arg0, arg1, arg2);
}

char* Pure_g_ascii_formatd(char* arg0, int arg1, char const* arg2, double arg3)
{
  return g_ascii_formatd(arg0, arg1, arg2, arg3);
}

char* Pure_g_strchug(char* arg0)
{
  return g_strchug(arg0);
}

char* Pure_g_strchomp(char* arg0)
{
  return g_strchomp(arg0);
}

int Pure_g_ascii_strcasecmp(char const* arg0, char const* arg1)
{
  return g_ascii_strcasecmp(arg0, arg1);
}

int Pure_g_ascii_strncasecmp(char const* arg0, char const* arg1, unsigned long arg2)
{
  return g_ascii_strncasecmp(arg0, arg1, arg2);
}

char* Pure_g_ascii_strdown(char const* arg0, long arg1)
{
  return g_ascii_strdown(arg0, arg1);
}

char* Pure_g_ascii_strup(char const* arg0, long arg1)
{
  return g_ascii_strup(arg0, arg1);
}

int Pure_g_strcasecmp(char const* arg0, char const* arg1)
{
  return g_strcasecmp(arg0, arg1);
}

int Pure_g_strncasecmp(char const* arg0, char const* arg1, unsigned int arg2)
{
  return g_strncasecmp(arg0, arg1, arg2);
}

char* Pure_g_strdown(char* arg0)
{
  return g_strdown(arg0);
}

char* Pure_g_strup(char* arg0)
{
  return g_strup(arg0);
}

char* Pure_g_strdup(char const* arg0)
{
  return g_strdup(arg0);
}

char* Pure_g_strdup_printf(char const* arg0)
{
  return g_strdup_printf(arg0);
}

char* Pure_g_strdup_vprintf(char const* arg0, void* arg1)
{
  return g_strdup_vprintf(arg0, arg1);
}

char* Pure_g_strndup(char const* arg0, unsigned long arg1)
{
  return g_strndup(arg0, arg1);
}

char* Pure_g_strnfill(unsigned long arg0, char arg1)
{
  return g_strnfill(arg0, arg1);
}

char* Pure_g_strconcat(char const* arg0)
{
  return g_strconcat(arg0);
}

char* Pure_g_strjoin(char const* arg0)
{
  return g_strjoin(arg0);
}

char* Pure_g_strcompress(char const* arg0)
{
  return g_strcompress(arg0);
}

char* Pure_g_strescape(char const* arg0, char const* arg1)
{
  return g_strescape(arg0, arg1);
}

void* Pure_g_memdup(void const* arg0, unsigned int arg1)
{
  return g_memdup(arg0, arg1);
}

char** Pure_g_strsplit(char const* arg0, char const* arg1, int arg2)
{
  return g_strsplit(arg0, arg1, arg2);
}

char** Pure_g_strsplit_set(char const* arg0, char const* arg1, int arg2)
{
  return g_strsplit_set(arg0, arg1, arg2);
}

char* Pure_g_strjoinv(char const* arg0, char** arg1)
{
  return g_strjoinv(arg0, arg1);
}

void Pure_g_strfreev(char** arg0)
{
  return g_strfreev(arg0);
}

char** Pure_g_strdupv(char** arg0)
{
  return g_strdupv(arg0);
}

unsigned int Pure_g_strv_length(char** arg0)
{
  return g_strv_length(arg0);
}

char* Pure_g_stpcpy(char* arg0, char const* arg1)
{
  return g_stpcpy(arg0, arg1);
}

char const* Pure_g_strip_context(char const* arg0, char const* arg1)
{
  return g_strip_context(arg0, arg1);
}

char const* Pure_g_dgettext(char const* arg0, char const* arg1)
{
  return g_dgettext(arg0, arg1);
}

char const* Pure_g_dngettext(char const* arg0, char const* arg1, char const* arg2, unsigned long arg3)
{
  return g_dngettext(arg0, arg1, arg2, arg3);
}

char const* Pure_g_dpgettext(char const* arg0, char const* arg1, unsigned long arg2)
{
  return g_dpgettext(arg0, arg1, arg2);
}

char const* Pure_g_dpgettext2(char const* arg0, char const* arg1, char const* arg2)
{
  return g_dpgettext2(arg0, arg1, arg2);
}

int Pure_g_strcmp0(char const* arg0, char const* arg1)
{
  return g_strcmp0(arg0, arg1);
}

void Pure_g_test_minimized_result(double arg0, char const* arg1)
{
  return g_test_minimized_result(arg0, arg1);
}

void Pure_g_test_maximized_result(double arg0, char const* arg1)
{
  return g_test_maximized_result(arg0, arg1);
}

void Pure_g_test_init(int* arg0, char*** arg1)
{
  return g_test_init(arg0, arg1);
}

int Pure_g_test_run()
{
  return g_test_run();
}

void Pure_g_test_add_func(char const* arg0, void* arg1)
{
  return g_test_add_func(arg0, arg1);
}

void Pure_g_test_add_data_func(char const* arg0, void const* arg1, void* arg2)
{
  return g_test_add_data_func(arg0, arg1, arg2);
}

void Pure_g_test_message(char const* arg0)
{
  return g_test_message(arg0);
}

void Pure_g_test_bug_base(char const* arg0)
{
  return g_test_bug_base(arg0);
}

void Pure_g_test_bug(char const* arg0)
{
  return g_test_bug(arg0);
}

void Pure_g_test_timer_start()
{
  return g_test_timer_start();
}

double Pure_g_test_timer_elapsed()
{
  return g_test_timer_elapsed();
}

double Pure_g_test_timer_last()
{
  return g_test_timer_last();
}

void Pure_g_test_queue_free(void* arg0)
{
  return g_test_queue_free(arg0);
}

void Pure_g_test_queue_destroy(void* arg0, void* arg1)
{
  return g_test_queue_destroy(arg0, arg1);
}

int Pure_g_test_trap_fork(unsigned long arg0, unsigned int arg1)
{
  return g_test_trap_fork(arg0, arg1);
}

int Pure_g_test_trap_has_passed()
{
  return g_test_trap_has_passed();
}

int Pure_g_test_trap_reached_timeout()
{
  return g_test_trap_reached_timeout();
}

int Pure_g_test_rand_int()
{
  return g_test_rand_int();
}

int Pure_g_test_rand_int_range(int arg0, int arg1)
{
  return g_test_rand_int_range(arg0, arg1);
}

double Pure_g_test_rand_double()
{
  return g_test_rand_double();
}

double Pure_g_test_rand_double_range(double arg0, double arg1)
{
  return g_test_rand_double_range(arg0, arg1);
}

GTestCase* Pure_g_test_create_case(char const* arg0, unsigned long arg1, void const* arg2, void* arg3, void* arg4, void* arg5)
{
  return g_test_create_case(arg0, arg1, arg2, arg3, arg4, arg5);
}

GTestSuite* Pure_g_test_create_suite(char const* arg0)
{
  return g_test_create_suite(arg0);
}

GTestSuite* Pure_g_test_get_root()
{
  return g_test_get_root();
}

void Pure_g_test_suite_add(GTestSuite* arg0, GTestCase* arg1)
{
  return g_test_suite_add(arg0, arg1);
}

void Pure_g_test_suite_add_suite(GTestSuite* arg0, GTestSuite* arg1)
{
  return g_test_suite_add_suite(arg0, arg1);
}

int Pure_g_test_run_suite(GTestSuite* arg0)
{
  return g_test_run_suite(arg0);
}

void Pure_g_test_trap_assertions(char const* arg0, char const* arg1, int arg2, char const* arg3, unsigned long arg4, char const* arg5)
{
  return g_test_trap_assertions(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_assertion_message(char const* arg0, char const* arg1, int arg2, char const* arg3, char const* arg4)
{
  return g_assertion_message(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_assertion_message_expr(char const* arg0, char const* arg1, int arg2, char const* arg3, char const* arg4)
{
  return g_assertion_message_expr(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_assertion_message_cmpstr(char const* arg0, char const* arg1, int arg2, char const* arg3, char const* arg4, char const* arg5, char const* arg6, char const* arg7)
{
  return g_assertion_message_cmpstr(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

void Pure_g_assertion_message_cmpnum(char const* arg0, char const* arg1, int arg2, char const* arg3, char const* arg4, double arg5, char const* arg6, double arg7, char arg8)
{
  return g_assertion_message_cmpnum(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_g_test_add_vtable(char const* arg0, unsigned long arg1, void const* arg2, void* arg3, void* arg4, void* arg5)
{
  return g_test_add_vtable(arg0, arg1, arg2, arg3, arg4, arg5);
}

char const* Pure_g_test_log_type_name(unsigned int arg0)
{
  return g_test_log_type_name(arg0);
}

GTestLogBuffer* Pure_g_test_log_buffer_new()
{
  return g_test_log_buffer_new();
}

void Pure_g_test_log_buffer_free(GTestLogBuffer* arg0)
{
  return g_test_log_buffer_free(arg0);
}

void Pure_g_test_log_buffer_push(GTestLogBuffer* arg0, unsigned int arg1, unsigned char const* arg2)
{
  return g_test_log_buffer_push(arg0, arg1, arg2);
}

GTestLogMsg* Pure_g_test_log_buffer_pop(GTestLogBuffer* arg0)
{
  return g_test_log_buffer_pop(arg0);
}

void Pure_g_test_log_msg_free(GTestLogMsg* arg0)
{
  return g_test_log_msg_free(arg0);
}

GThreadPool* Pure_g_thread_pool_new(void* arg0, void* arg1, int arg2, int arg3, GError** arg4)
{
  return g_thread_pool_new(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_thread_pool_push(GThreadPool* arg0, void* arg1, GError** arg2)
{
  return g_thread_pool_push(arg0, arg1, arg2);
}

void Pure_g_thread_pool_set_max_threads(GThreadPool* arg0, int arg1, GError** arg2)
{
  return g_thread_pool_set_max_threads(arg0, arg1, arg2);
}

int Pure_g_thread_pool_get_max_threads(GThreadPool* arg0)
{
  return g_thread_pool_get_max_threads(arg0);
}

unsigned int Pure_g_thread_pool_get_num_threads(GThreadPool* arg0)
{
  return g_thread_pool_get_num_threads(arg0);
}

unsigned int Pure_g_thread_pool_unprocessed(GThreadPool* arg0)
{
  return g_thread_pool_unprocessed(arg0);
}

void Pure_g_thread_pool_free(GThreadPool* arg0, int arg1, int arg2)
{
  return g_thread_pool_free(arg0, arg1, arg2);
}

void Pure_g_thread_pool_set_max_unused_threads(int arg0)
{
  return g_thread_pool_set_max_unused_threads(arg0);
}

int Pure_g_thread_pool_get_max_unused_threads()
{
  return g_thread_pool_get_max_unused_threads();
}

unsigned int Pure_g_thread_pool_get_num_unused_threads()
{
  return g_thread_pool_get_num_unused_threads();
}

void Pure_g_thread_pool_stop_unused_threads()
{
  return g_thread_pool_stop_unused_threads();
}

void Pure_g_thread_pool_set_sort_function(GThreadPool* arg0, void* arg1, void* arg2)
{
  return g_thread_pool_set_sort_function(arg0, arg1, arg2);
}

void Pure_g_thread_pool_set_max_idle_time(unsigned int arg0)
{
  return g_thread_pool_set_max_idle_time(arg0);
}

unsigned int Pure_g_thread_pool_get_max_idle_time()
{
  return g_thread_pool_get_max_idle_time();
}

GTimer* Pure_g_timer_new()
{
  return g_timer_new();
}

void Pure_g_timer_destroy(GTimer* arg0)
{
  return g_timer_destroy(arg0);
}

void Pure_g_timer_start(GTimer* arg0)
{
  return g_timer_start(arg0);
}

void Pure_g_timer_stop(GTimer* arg0)
{
  return g_timer_stop(arg0);
}

void Pure_g_timer_reset(GTimer* arg0)
{
  return g_timer_reset(arg0);
}

void Pure_g_timer_continue(GTimer* arg0)
{
  return g_timer_continue(arg0);
}

double Pure_g_timer_elapsed(GTimer* arg0, unsigned long* arg1)
{
  return g_timer_elapsed(arg0, arg1);
}

void Pure_g_usleep(unsigned long arg0)
{
  return g_usleep(arg0);
}

void Pure_g_time_val_add(GTimeVal* arg0, long arg1)
{
  return g_time_val_add(arg0, arg1);
}

int Pure_g_time_val_from_iso8601(char const* arg0, GTimeVal* arg1)
{
  return g_time_val_from_iso8601(arg0, arg1);
}

char* Pure_g_time_val_to_iso8601(GTimeVal* arg0)
{
  return g_time_val_to_iso8601(arg0);
}

GTree* Pure_g_tree_new(void* arg0)
{
  return g_tree_new(arg0);
}

GTree* Pure_g_tree_new_with_data(void* arg0, void* arg1)
{
  return g_tree_new_with_data(arg0, arg1);
}

GTree* Pure_g_tree_new_full(void* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_tree_new_full(arg0, arg1, arg2, arg3);
}

void Pure_g_tree_destroy(GTree* arg0)
{
  return g_tree_destroy(arg0);
}

void Pure_g_tree_insert(GTree* arg0, void* arg1, void* arg2)
{
  return g_tree_insert(arg0, arg1, arg2);
}

void Pure_g_tree_replace(GTree* arg0, void* arg1, void* arg2)
{
  return g_tree_replace(arg0, arg1, arg2);
}

int Pure_g_tree_remove(GTree* arg0, void const* arg1)
{
  return g_tree_remove(arg0, arg1);
}

int Pure_g_tree_steal(GTree* arg0, void const* arg1)
{
  return g_tree_steal(arg0, arg1);
}

void* Pure_g_tree_lookup(GTree* arg0, void const* arg1)
{
  return g_tree_lookup(arg0, arg1);
}

int Pure_g_tree_lookup_extended(GTree* arg0, void const* arg1, void** arg2, void** arg3)
{
  return g_tree_lookup_extended(arg0, arg1, arg2, arg3);
}

void Pure_g_tree_foreach(GTree* arg0, void* arg1, void* arg2)
{
  return g_tree_foreach(arg0, arg1, arg2);
}

void Pure_g_tree_traverse(GTree* arg0, void* arg1, unsigned int arg2, void* arg3)
{
  return g_tree_traverse(arg0, arg1, arg2, arg3);
}

void* Pure_g_tree_search(GTree* arg0, void* arg1, void const* arg2)
{
  return g_tree_search(arg0, arg1, arg2);
}

int Pure_g_tree_height(GTree* arg0)
{
  return g_tree_height(arg0);
}

int Pure_g_tree_nnodes(GTree* arg0)
{
  return g_tree_nnodes(arg0);
}

char* Pure_g_uri_unescape_string(char const* arg0, char const* arg1)
{
  return g_uri_unescape_string(arg0, arg1);
}

char* Pure_g_uri_unescape_segment(char const* arg0, char const* arg1, char const* arg2)
{
  return g_uri_unescape_segment(arg0, arg1, arg2);
}

char* Pure_g_uri_parse_scheme(char const* arg0)
{
  return g_uri_parse_scheme(arg0);
}

char* Pure_g_uri_escape_string(char const* arg0, char const* arg1, int arg2)
{
  return g_uri_escape_string(arg0, arg1, arg2);
}
#include <glib-object.h>

void Pure_g_type_init()
{
  return g_type_init();
}

void Pure_g_type_init_with_debug_flags(unsigned int arg0)
{
  return g_type_init_with_debug_flags(arg0);
}

char const* Pure_g_type_name(unsigned long arg0)
{
  return g_type_name(arg0);
}

unsigned int Pure_g_type_qname(unsigned long arg0)
{
  return g_type_qname(arg0);
}

unsigned long Pure_g_type_from_name(char const* arg0)
{
  return g_type_from_name(arg0);
}

unsigned long Pure_g_type_parent(unsigned long arg0)
{
  return g_type_parent(arg0);
}

unsigned int Pure_g_type_depth(unsigned long arg0)
{
  return g_type_depth(arg0);
}

unsigned long Pure_g_type_next_base(unsigned long arg0, unsigned long arg1)
{
  return g_type_next_base(arg0, arg1);
}

int Pure_g_type_is_a(unsigned long arg0, unsigned long arg1)
{
  return g_type_is_a(arg0, arg1);
}

void* Pure_g_type_class_ref(unsigned long arg0)
{
  return g_type_class_ref(arg0);
}

void* Pure_g_type_class_peek(unsigned long arg0)
{
  return g_type_class_peek(arg0);
}

void* Pure_g_type_class_peek_static(unsigned long arg0)
{
  return g_type_class_peek_static(arg0);
}

void Pure_g_type_class_unref(void* arg0)
{
  return g_type_class_unref(arg0);
}

void* Pure_g_type_class_peek_parent(void* arg0)
{
  return g_type_class_peek_parent(arg0);
}

void* Pure_g_type_interface_peek(void* arg0, unsigned long arg1)
{
  return g_type_interface_peek(arg0, arg1);
}

void* Pure_g_type_interface_peek_parent(void* arg0)
{
  return g_type_interface_peek_parent(arg0);
}

void* Pure_g_type_default_interface_ref(unsigned long arg0)
{
  return g_type_default_interface_ref(arg0);
}

void* Pure_g_type_default_interface_peek(unsigned long arg0)
{
  return g_type_default_interface_peek(arg0);
}

void Pure_g_type_default_interface_unref(void* arg0)
{
  return g_type_default_interface_unref(arg0);
}

unsigned long* Pure_g_type_children(unsigned long arg0, unsigned int* arg1)
{
  return g_type_children(arg0, arg1);
}

unsigned long* Pure_g_type_interfaces(unsigned long arg0, unsigned int* arg1)
{
  return g_type_interfaces(arg0, arg1);
}

void Pure_g_type_set_qdata(unsigned long arg0, unsigned int arg1, void* arg2)
{
  return g_type_set_qdata(arg0, arg1, arg2);
}

void* Pure_g_type_get_qdata(unsigned long arg0, unsigned int arg1)
{
  return g_type_get_qdata(arg0, arg1);
}

void Pure_g_type_query(unsigned long arg0, GTypeQuery* arg1)
{
  return g_type_query(arg0, arg1);
}

unsigned long Pure_g_type_register_static(unsigned long arg0, char const* arg1, GTypeInfo const* arg2, unsigned int arg3)
{
  return g_type_register_static(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_type_register_static_simple(unsigned long arg0, char const* arg1, unsigned int arg2, void* arg3, unsigned int arg4, void* arg5, unsigned int arg6)
{
  return g_type_register_static_simple(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

unsigned long Pure_g_type_register_dynamic(unsigned long arg0, char const* arg1, GTypePlugin* arg2, unsigned int arg3)
{
  return g_type_register_dynamic(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_type_register_fundamental(unsigned long arg0, char const* arg1, GTypeInfo const* arg2, GTypeFundamentalInfo const* arg3, unsigned int arg4)
{
  return g_type_register_fundamental(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_type_add_interface_static(unsigned long arg0, unsigned long arg1, GInterfaceInfo const* arg2)
{
  return g_type_add_interface_static(arg0, arg1, arg2);
}

void Pure_g_type_add_interface_dynamic(unsigned long arg0, unsigned long arg1, GTypePlugin* arg2)
{
  return g_type_add_interface_dynamic(arg0, arg1, arg2);
}

void Pure_g_type_interface_add_prerequisite(unsigned long arg0, unsigned long arg1)
{
  return g_type_interface_add_prerequisite(arg0, arg1);
}

unsigned long* Pure_g_type_interface_prerequisites(unsigned long arg0, unsigned int* arg1)
{
  return g_type_interface_prerequisites(arg0, arg1);
}

void Pure_g_type_class_add_private(void* arg0, unsigned long arg1)
{
  return g_type_class_add_private(arg0, arg1);
}

void* Pure_g_type_instance_get_private(GTypeInstance* arg0, unsigned long arg1)
{
  return g_type_instance_get_private(arg0, arg1);
}

GTypePlugin* Pure_g_type_get_plugin(unsigned long arg0)
{
  return g_type_get_plugin(arg0);
}

GTypePlugin* Pure_g_type_interface_get_plugin(unsigned long arg0, unsigned long arg1)
{
  return g_type_interface_get_plugin(arg0, arg1);
}

unsigned long Pure_g_type_fundamental_next()
{
  return g_type_fundamental_next();
}

unsigned long Pure_g_type_fundamental(unsigned long arg0)
{
  return g_type_fundamental(arg0);
}

GTypeInstance* Pure_g_type_create_instance(unsigned long arg0)
{
  return g_type_create_instance(arg0);
}

void Pure_g_type_free_instance(GTypeInstance* arg0)
{
  return g_type_free_instance(arg0);
}

void Pure_g_type_add_class_cache_func(void* arg0, void* arg1)
{
  return g_type_add_class_cache_func(arg0, arg1);
}

void Pure_g_type_remove_class_cache_func(void* arg0, void* arg1)
{
  return g_type_remove_class_cache_func(arg0, arg1);
}

void Pure_g_type_class_unref_uncached(void* arg0)
{
  return g_type_class_unref_uncached(arg0);
}

void Pure_g_type_add_interface_check(void* arg0, void* arg1)
{
  return g_type_add_interface_check(arg0, arg1);
}

void Pure_g_type_remove_interface_check(void* arg0, void* arg1)
{
  return g_type_remove_interface_check(arg0, arg1);
}

GTypeValueTable* Pure_g_type_value_table_peek(unsigned long arg0)
{
  return g_type_value_table_peek(arg0);
}

int Pure_g_type_check_instance(GTypeInstance* arg0)
{
  return g_type_check_instance(arg0);
}

GTypeInstance* Pure_g_type_check_instance_cast(GTypeInstance* arg0, unsigned long arg1)
{
  return g_type_check_instance_cast(arg0, arg1);
}

int Pure_g_type_check_instance_is_a(GTypeInstance* arg0, unsigned long arg1)
{
  return g_type_check_instance_is_a(arg0, arg1);
}

GTypeClass* Pure_g_type_check_class_cast(GTypeClass* arg0, unsigned long arg1)
{
  return g_type_check_class_cast(arg0, arg1);
}

int Pure_g_type_check_class_is_a(GTypeClass* arg0, unsigned long arg1)
{
  return g_type_check_class_is_a(arg0, arg1);
}

int Pure_g_type_check_is_value_type(unsigned long arg0)
{
  return g_type_check_is_value_type(arg0);
}

int Pure_g_type_check_value(GValue* arg0)
{
  return g_type_check_value(arg0);
}

int Pure_g_type_check_value_holds(GValue* arg0, unsigned long arg1)
{
  return g_type_check_value_holds(arg0, arg1);
}

int Pure_g_type_test_flags(unsigned long arg0, unsigned int arg1)
{
  return g_type_test_flags(arg0, arg1);
}

char const* Pure_g_type_name_from_instance(GTypeInstance* arg0)
{
  return g_type_name_from_instance(arg0);
}

char const* Pure_g_type_name_from_class(GTypeClass* arg0)
{
  return g_type_name_from_class(arg0);
}

void* Pure_g_boxed_copy(unsigned long arg0, void const* arg1)
{
  return g_boxed_copy(arg0, arg1);
}

void Pure_g_boxed_free(unsigned long arg0, void* arg1)
{
  return g_boxed_free(arg0, arg1);
}

void Pure_g_value_set_boxed(GValue* arg0, void const* arg1)
{
  return g_value_set_boxed(arg0, arg1);
}

void Pure_g_value_set_static_boxed(GValue* arg0, void const* arg1)
{
  return g_value_set_static_boxed(arg0, arg1);
}

void* Pure_g_value_get_boxed(GValue const* arg0)
{
  return g_value_get_boxed(arg0);
}

void* Pure_g_value_dup_boxed(GValue const* arg0)
{
  return g_value_dup_boxed(arg0);
}

unsigned long Pure_g_boxed_type_register_static(char const* arg0, void* arg1, void* arg2)
{
  return g_boxed_type_register_static(arg0, arg1, arg2);
}

void Pure_g_value_take_boxed(GValue* arg0, void const* arg1)
{
  return g_value_take_boxed(arg0, arg1);
}

void Pure_g_value_set_boxed_take_ownership(GValue* arg0, void const* arg1)
{
  return g_value_set_boxed_take_ownership(arg0, arg1);
}

unsigned long Pure_g_closure_get_type()
{
  return g_closure_get_type();
}

unsigned long Pure_g_value_get_type()
{
  return g_value_get_type();
}

unsigned long Pure_g_value_array_get_type()
{
  return g_value_array_get_type();
}

unsigned long Pure_g_date_get_type()
{
  return g_date_get_type();
}

unsigned long Pure_g_strv_get_type()
{
  return g_strv_get_type();
}

unsigned long Pure_g_gstring_get_type()
{
  return g_gstring_get_type();
}

unsigned long Pure_g_hash_table_get_type()
{
  return g_hash_table_get_type();
}

unsigned long Pure_g_regex_get_type()
{
  return g_regex_get_type();
}

GEnumValue* Pure_g_enum_get_value(GEnumClass* arg0, int arg1)
{
  return g_enum_get_value(arg0, arg1);
}

GEnumValue* Pure_g_enum_get_value_by_name(GEnumClass* arg0, char const* arg1)
{
  return g_enum_get_value_by_name(arg0, arg1);
}

GEnumValue* Pure_g_enum_get_value_by_nick(GEnumClass* arg0, char const* arg1)
{
  return g_enum_get_value_by_nick(arg0, arg1);
}

GFlagsValue* Pure_g_flags_get_first_value(GFlagsClass* arg0, unsigned int arg1)
{
  return g_flags_get_first_value(arg0, arg1);
}

GFlagsValue* Pure_g_flags_get_value_by_name(GFlagsClass* arg0, char const* arg1)
{
  return g_flags_get_value_by_name(arg0, arg1);
}

GFlagsValue* Pure_g_flags_get_value_by_nick(GFlagsClass* arg0, char const* arg1)
{
  return g_flags_get_value_by_nick(arg0, arg1);
}

void Pure_g_value_set_enum(GValue* arg0, int arg1)
{
  return g_value_set_enum(arg0, arg1);
}

int Pure_g_value_get_enum(GValue const* arg0)
{
  return g_value_get_enum(arg0);
}

void Pure_g_value_set_flags(GValue* arg0, unsigned int arg1)
{
  return g_value_set_flags(arg0, arg1);
}

unsigned int Pure_g_value_get_flags(GValue const* arg0)
{
  return g_value_get_flags(arg0);
}

unsigned long Pure_g_enum_register_static(char const* arg0, GEnumValue const* arg1)
{
  return g_enum_register_static(arg0, arg1);
}

unsigned long Pure_g_flags_register_static(char const* arg0, GFlagsValue const* arg1)
{
  return g_flags_register_static(arg0, arg1);
}

void Pure_g_enum_complete_type_info(unsigned long arg0, GTypeInfo* arg1, GEnumValue const* arg2)
{
  return g_enum_complete_type_info(arg0, arg1, arg2);
}

void Pure_g_flags_complete_type_info(unsigned long arg0, GTypeInfo* arg1, GFlagsValue const* arg2)
{
  return g_flags_complete_type_info(arg0, arg1, arg2);
}

GValue* Pure_g_value_init(GValue* arg0, unsigned long arg1)
{
  return g_value_init(arg0, arg1);
}

void Pure_g_value_copy(GValue const* arg0, GValue* arg1)
{
  return g_value_copy(arg0, arg1);
}

GValue* Pure_g_value_reset(GValue* arg0)
{
  return g_value_reset(arg0);
}

void Pure_g_value_unset(GValue* arg0)
{
  return g_value_unset(arg0);
}

void Pure_g_value_set_instance(GValue* arg0, void* arg1)
{
  return g_value_set_instance(arg0, arg1);
}

int Pure_g_value_fits_pointer(GValue const* arg0)
{
  return g_value_fits_pointer(arg0);
}

void* Pure_g_value_peek_pointer(GValue const* arg0)
{
  return g_value_peek_pointer(arg0);
}

int Pure_g_value_type_compatible(unsigned long arg0, unsigned long arg1)
{
  return g_value_type_compatible(arg0, arg1);
}

int Pure_g_value_type_transformable(unsigned long arg0, unsigned long arg1)
{
  return g_value_type_transformable(arg0, arg1);
}

int Pure_g_value_transform(GValue const* arg0, GValue* arg1)
{
  return g_value_transform(arg0, arg1);
}

void Pure_g_value_register_transform_func(unsigned long arg0, unsigned long arg1, void* arg2)
{
  return g_value_register_transform_func(arg0, arg1, arg2);
}

GParamSpec* Pure_g_param_spec_ref(GParamSpec* arg0)
{
  return g_param_spec_ref(arg0);
}

void Pure_g_param_spec_unref(GParamSpec* arg0)
{
  return g_param_spec_unref(arg0);
}

void Pure_g_param_spec_sink(GParamSpec* arg0)
{
  return g_param_spec_sink(arg0);
}

GParamSpec* Pure_g_param_spec_ref_sink(GParamSpec* arg0)
{
  return g_param_spec_ref_sink(arg0);
}

void* Pure_g_param_spec_get_qdata(GParamSpec* arg0, unsigned int arg1)
{
  return g_param_spec_get_qdata(arg0, arg1);
}

void Pure_g_param_spec_set_qdata(GParamSpec* arg0, unsigned int arg1, void* arg2)
{
  return g_param_spec_set_qdata(arg0, arg1, arg2);
}

void Pure_g_param_spec_set_qdata_full(GParamSpec* arg0, unsigned int arg1, void* arg2, void* arg3)
{
  return g_param_spec_set_qdata_full(arg0, arg1, arg2, arg3);
}

void* Pure_g_param_spec_steal_qdata(GParamSpec* arg0, unsigned int arg1)
{
  return g_param_spec_steal_qdata(arg0, arg1);
}

GParamSpec* Pure_g_param_spec_get_redirect_target(GParamSpec* arg0)
{
  return g_param_spec_get_redirect_target(arg0);
}

void Pure_g_param_value_set_default(GParamSpec* arg0, GValue* arg1)
{
  return g_param_value_set_default(arg0, arg1);
}

int Pure_g_param_value_defaults(GParamSpec* arg0, GValue* arg1)
{
  return g_param_value_defaults(arg0, arg1);
}

int Pure_g_param_value_validate(GParamSpec* arg0, GValue* arg1)
{
  return g_param_value_validate(arg0, arg1);
}

int Pure_g_param_value_convert(GParamSpec* arg0, GValue const* arg1, GValue* arg2, int arg3)
{
  return g_param_value_convert(arg0, arg1, arg2, arg3);
}

int Pure_g_param_values_cmp(GParamSpec* arg0, GValue const* arg1, GValue const* arg2)
{
  return g_param_values_cmp(arg0, arg1, arg2);
}

char const* Pure_g_param_spec_get_name(GParamSpec* arg0)
{
  return g_param_spec_get_name(arg0);
}

char const* Pure_g_param_spec_get_nick(GParamSpec* arg0)
{
  return g_param_spec_get_nick(arg0);
}

char const* Pure_g_param_spec_get_blurb(GParamSpec* arg0)
{
  return g_param_spec_get_blurb(arg0);
}

void Pure_g_value_set_param(GValue* arg0, GParamSpec* arg1)
{
  return g_value_set_param(arg0, arg1);
}

GParamSpec* Pure_g_value_get_param(GValue const* arg0)
{
  return g_value_get_param(arg0);
}

GParamSpec* Pure_g_value_dup_param(GValue const* arg0)
{
  return g_value_dup_param(arg0);
}

void Pure_g_value_take_param(GValue* arg0, GParamSpec* arg1)
{
  return g_value_take_param(arg0, arg1);
}

void Pure_g_value_set_param_take_ownership(GValue* arg0, GParamSpec* arg1)
{
  return g_value_set_param_take_ownership(arg0, arg1);
}

unsigned long Pure_g_param_type_register_static(char const* arg0, GParamSpecTypeInfo const* arg1)
{
  return g_param_type_register_static(arg0, arg1);
}

void* Pure_g_param_spec_internal(unsigned long arg0, char const* arg1, char const* arg2, char const* arg3, unsigned int arg4)
{
  return g_param_spec_internal(arg0, arg1, arg2, arg3, arg4);
}

GParamSpecPool* Pure_g_param_spec_pool_new(int arg0)
{
  return g_param_spec_pool_new(arg0);
}

void Pure_g_param_spec_pool_insert(GParamSpecPool* arg0, GParamSpec* arg1, unsigned long arg2)
{
  return g_param_spec_pool_insert(arg0, arg1, arg2);
}

void Pure_g_param_spec_pool_remove(GParamSpecPool* arg0, GParamSpec* arg1)
{
  return g_param_spec_pool_remove(arg0, arg1);
}

GParamSpec* Pure_g_param_spec_pool_lookup(GParamSpecPool* arg0, char const* arg1, unsigned long arg2, int arg3)
{
  return g_param_spec_pool_lookup(arg0, arg1, arg2, arg3);
}

GList* Pure_g_param_spec_pool_list_owned(GParamSpecPool* arg0, unsigned long arg1)
{
  return g_param_spec_pool_list_owned(arg0, arg1);
}

GParamSpec** Pure_g_param_spec_pool_list(GParamSpecPool* arg0, unsigned long arg1, unsigned int* arg2)
{
  return g_param_spec_pool_list(arg0, arg1, arg2);
}

GClosure* Pure_g_cclosure_new(void* arg0, void* arg1, void* arg2)
{
  return g_cclosure_new(arg0, arg1, arg2);
}

GClosure* Pure_g_cclosure_new_swap(void* arg0, void* arg1, void* arg2)
{
  return g_cclosure_new_swap(arg0, arg1, arg2);
}

GClosure* Pure_g_signal_type_cclosure_new(unsigned long arg0, unsigned int arg1)
{
  return g_signal_type_cclosure_new(arg0, arg1);
}

GClosure* Pure_g_closure_ref(GClosure* arg0)
{
  return g_closure_ref(arg0);
}

void Pure_g_closure_sink(GClosure* arg0)
{
  return g_closure_sink(arg0);
}

void Pure_g_closure_unref(GClosure* arg0)
{
  return g_closure_unref(arg0);
}

GClosure* Pure_g_closure_new_simple(unsigned int arg0, void* arg1)
{
  return g_closure_new_simple(arg0, arg1);
}

void Pure_g_closure_add_finalize_notifier(GClosure* arg0, void* arg1, void* arg2)
{
  return g_closure_add_finalize_notifier(arg0, arg1, arg2);
}

void Pure_g_closure_remove_finalize_notifier(GClosure* arg0, void* arg1, void* arg2)
{
  return g_closure_remove_finalize_notifier(arg0, arg1, arg2);
}

void Pure_g_closure_add_invalidate_notifier(GClosure* arg0, void* arg1, void* arg2)
{
  return g_closure_add_invalidate_notifier(arg0, arg1, arg2);
}

void Pure_g_closure_remove_invalidate_notifier(GClosure* arg0, void* arg1, void* arg2)
{
  return g_closure_remove_invalidate_notifier(arg0, arg1, arg2);
}

void Pure_g_closure_add_marshal_guards(GClosure* arg0, void* arg1, void* arg2, void* arg3, void* arg4)
{
  return g_closure_add_marshal_guards(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_closure_set_marshal(GClosure* arg0, void* arg1)
{
  return g_closure_set_marshal(arg0, arg1);
}

void Pure_g_closure_set_meta_marshal(GClosure* arg0, void* arg1, void* arg2)
{
  return g_closure_set_meta_marshal(arg0, arg1, arg2);
}

void Pure_g_closure_invalidate(GClosure* arg0)
{
  return g_closure_invalidate(arg0);
}

void Pure_g_closure_invoke(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4)
{
  return g_closure_invoke(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_cclosure_marshal_VOID__VOID(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__VOID(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__BOOLEAN(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__BOOLEAN(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__CHAR(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__CHAR(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__UCHAR(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__UCHAR(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__INT(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__INT(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__UINT(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__UINT(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__LONG(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__LONG(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__ULONG(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__ULONG(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__ENUM(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__ENUM(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__FLAGS(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__FLAGS(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__FLOAT(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__FLOAT(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__DOUBLE(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__DOUBLE(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__STRING(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__STRING(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__PARAM(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__PARAM(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__BOXED(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__BOXED(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__POINTER(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__POINTER(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__OBJECT(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__OBJECT(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_VOID__UINT_POINTER(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_VOID__UINT_POINTER(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_BOOLEAN__FLAGS(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_BOOLEAN__FLAGS(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_cclosure_marshal_STRING__OBJECT_POINTER(GClosure* arg0, GValue* arg1, unsigned int arg2, GValue const* arg3, void* arg4, void* arg5)
{
  return g_cclosure_marshal_STRING__OBJECT_POINTER(arg0, arg1, arg2, arg3, arg4, arg5);
}

unsigned int Pure_g_signal_newv(char const* arg0, unsigned long arg1, unsigned int arg2, GClosure* arg3, void* arg4, void* arg5, void* arg6, unsigned long arg7, unsigned int arg8, unsigned long* arg9)
{
  return g_signal_newv(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

unsigned int Pure_g_signal_new_valist(char const* arg0, unsigned long arg1, unsigned int arg2, GClosure* arg3, void* arg4, void* arg5, void* arg6, unsigned long arg7, unsigned int arg8, void* arg9)
{
  return g_signal_new_valist(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

unsigned int Pure_g_signal_new(char const* arg0, unsigned long arg1, unsigned int arg2, unsigned int arg3, void* arg4, void* arg5, void* arg6, unsigned long arg7, unsigned int arg8)
{
  return g_signal_new(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

unsigned int Pure_g_signal_new_class_handler(char const* arg0, unsigned long arg1, unsigned int arg2, void* arg3, void* arg4, void* arg5, void* arg6, unsigned long arg7, unsigned int arg8)
{
  return g_signal_new_class_handler(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_g_signal_emitv(GValue const* arg0, unsigned int arg1, unsigned int arg2, GValue* arg3)
{
  return g_signal_emitv(arg0, arg1, arg2, arg3);
}

void Pure_g_signal_emit_valist(void* arg0, unsigned int arg1, unsigned int arg2, void* arg3)
{
  return g_signal_emit_valist(arg0, arg1, arg2, arg3);
}

void Pure_g_signal_emit(void* arg0, unsigned int arg1, unsigned int arg2)
{
  return g_signal_emit(arg0, arg1, arg2);
}

void Pure_g_signal_emit_by_name(void* arg0, char const* arg1)
{
  return g_signal_emit_by_name(arg0, arg1);
}

unsigned int Pure_g_signal_lookup(char const* arg0, unsigned long arg1)
{
  return g_signal_lookup(arg0, arg1);
}

char const* Pure_g_signal_name(unsigned int arg0)
{
  return g_signal_name(arg0);
}

void Pure_g_signal_query(unsigned int arg0, GSignalQuery* arg1)
{
  return g_signal_query(arg0, arg1);
}

unsigned int* Pure_g_signal_list_ids(unsigned long arg0, unsigned int* arg1)
{
  return g_signal_list_ids(arg0, arg1);
}

int Pure_g_signal_parse_name(char const* arg0, unsigned long arg1, unsigned int* arg2, unsigned int* arg3, int arg4)
{
  return g_signal_parse_name(arg0, arg1, arg2, arg3, arg4);
}

GSignalInvocationHint* Pure_g_signal_get_invocation_hint(void* arg0)
{
  return g_signal_get_invocation_hint(arg0);
}

void Pure_g_signal_stop_emission(void* arg0, unsigned int arg1, unsigned int arg2)
{
  return g_signal_stop_emission(arg0, arg1, arg2);
}

void Pure_g_signal_stop_emission_by_name(void* arg0, char const* arg1)
{
  return g_signal_stop_emission_by_name(arg0, arg1);
}

unsigned long Pure_g_signal_add_emission_hook(unsigned int arg0, unsigned int arg1, void* arg2, void* arg3, void* arg4)
{
  return g_signal_add_emission_hook(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_signal_remove_emission_hook(unsigned int arg0, unsigned long arg1)
{
  return g_signal_remove_emission_hook(arg0, arg1);
}

int Pure_g_signal_has_handler_pending(void* arg0, unsigned int arg1, unsigned int arg2, int arg3)
{
  return g_signal_has_handler_pending(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_signal_connect_closure_by_id(void* arg0, unsigned int arg1, unsigned int arg2, GClosure* arg3, int arg4)
{
  return g_signal_connect_closure_by_id(arg0, arg1, arg2, arg3, arg4);
}

unsigned long Pure_g_signal_connect_closure(void* arg0, char const* arg1, GClosure* arg2, int arg3)
{
  return g_signal_connect_closure(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_signal_connect_data(void* arg0, char const* arg1, void* arg2, void* arg3, void* arg4, unsigned int arg5)
{
  return g_signal_connect_data(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_signal_handler_block(void* arg0, unsigned long arg1)
{
  return g_signal_handler_block(arg0, arg1);
}

void Pure_g_signal_handler_unblock(void* arg0, unsigned long arg1)
{
  return g_signal_handler_unblock(arg0, arg1);
}

void Pure_g_signal_handler_disconnect(void* arg0, unsigned long arg1)
{
  return g_signal_handler_disconnect(arg0, arg1);
}

int Pure_g_signal_handler_is_connected(void* arg0, unsigned long arg1)
{
  return g_signal_handler_is_connected(arg0, arg1);
}

unsigned long Pure_g_signal_handler_find(void* arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, GClosure* arg4, void* arg5, void* arg6)
{
  return g_signal_handler_find(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

unsigned int Pure_g_signal_handlers_block_matched(void* arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, GClosure* arg4, void* arg5, void* arg6)
{
  return g_signal_handlers_block_matched(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

unsigned int Pure_g_signal_handlers_unblock_matched(void* arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, GClosure* arg4, void* arg5, void* arg6)
{
  return g_signal_handlers_unblock_matched(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

unsigned int Pure_g_signal_handlers_disconnect_matched(void* arg0, unsigned int arg1, unsigned int arg2, unsigned int arg3, GClosure* arg4, void* arg5, void* arg6)
{
  return g_signal_handlers_disconnect_matched(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_g_signal_override_class_closure(unsigned int arg0, unsigned long arg1, GClosure* arg2)
{
  return g_signal_override_class_closure(arg0, arg1, arg2);
}

void Pure_g_signal_override_class_handler(char const* arg0, unsigned long arg1, void* arg2)
{
  return g_signal_override_class_handler(arg0, arg1, arg2);
}

void Pure_g_signal_chain_from_overridden(GValue const* arg0, GValue* arg1)
{
  return g_signal_chain_from_overridden(arg0, arg1);
}

void Pure_g_signal_chain_from_overridden_handler(void* arg0)
{
  return g_signal_chain_from_overridden_handler(arg0);
}

int Pure_g_signal_accumulator_true_handled(GSignalInvocationHint* arg0, GValue* arg1, GValue const* arg2, void* arg3)
{
  return g_signal_accumulator_true_handled(arg0, arg1, arg2, arg3);
}

void Pure_g_signal_handlers_destroy(void* arg0)
{
  return g_signal_handlers_destroy(arg0);
}

unsigned long Pure_g_initially_unowned_get_type()
{
  return g_initially_unowned_get_type();
}

void Pure_g_object_class_install_property(GObjectClass* arg0, unsigned int arg1, GParamSpec* arg2)
{
  return g_object_class_install_property(arg0, arg1, arg2);
}

GParamSpec* Pure_g_object_class_find_property(GObjectClass* arg0, char const* arg1)
{
  return g_object_class_find_property(arg0, arg1);
}

GParamSpec** Pure_g_object_class_list_properties(GObjectClass* arg0, unsigned int* arg1)
{
  return g_object_class_list_properties(arg0, arg1);
}

void Pure_g_object_class_override_property(GObjectClass* arg0, unsigned int arg1, char const* arg2)
{
  return g_object_class_override_property(arg0, arg1, arg2);
}

void Pure_g_object_interface_install_property(void* arg0, GParamSpec* arg1)
{
  return g_object_interface_install_property(arg0, arg1);
}

GParamSpec* Pure_g_object_interface_find_property(void* arg0, char const* arg1)
{
  return g_object_interface_find_property(arg0, arg1);
}

GParamSpec** Pure_g_object_interface_list_properties(void* arg0, unsigned int* arg1)
{
  return g_object_interface_list_properties(arg0, arg1);
}

unsigned long Pure_g_object_get_type()
{
  return g_object_get_type();
}

void* Pure_g_object_new(unsigned long arg0, char const* arg1)
{
  return g_object_new(arg0, arg1);
}

void* Pure_g_object_newv(unsigned long arg0, unsigned int arg1, GParameter* arg2)
{
  return g_object_newv(arg0, arg1, arg2);
}

GObject* Pure_g_object_new_valist(unsigned long arg0, char const* arg1, void* arg2)
{
  return g_object_new_valist(arg0, arg1, arg2);
}

void Pure_g_object_set(void* arg0, char const* arg1)
{
  return g_object_set(arg0, arg1);
}

void Pure_g_object_get(void* arg0, char const* arg1)
{
  return g_object_get(arg0, arg1);
}

void* Pure_g_object_connect(void* arg0, char const* arg1)
{
  return g_object_connect(arg0, arg1);
}

void Pure_g_object_disconnect(void* arg0, char const* arg1)
{
  return g_object_disconnect(arg0, arg1);
}

void Pure_g_object_set_valist(GObject* arg0, char const* arg1, void* arg2)
{
  return g_object_set_valist(arg0, arg1, arg2);
}

void Pure_g_object_get_valist(GObject* arg0, char const* arg1, void* arg2)
{
  return g_object_get_valist(arg0, arg1, arg2);
}

void Pure_g_object_set_property(GObject* arg0, char const* arg1, GValue const* arg2)
{
  return g_object_set_property(arg0, arg1, arg2);
}

void Pure_g_object_get_property(GObject* arg0, char const* arg1, GValue* arg2)
{
  return g_object_get_property(arg0, arg1, arg2);
}

void Pure_g_object_freeze_notify(GObject* arg0)
{
  return g_object_freeze_notify(arg0);
}

void Pure_g_object_notify(GObject* arg0, char const* arg1)
{
  return g_object_notify(arg0, arg1);
}

void Pure_g_object_thaw_notify(GObject* arg0)
{
  return g_object_thaw_notify(arg0);
}

int Pure_g_object_is_floating(void* arg0)
{
  return g_object_is_floating(arg0);
}

void* Pure_g_object_ref_sink(void* arg0)
{
  return g_object_ref_sink(arg0);
}

void* Pure_g_object_ref(void* arg0)
{
  return g_object_ref(arg0);
}

void Pure_g_object_unref(void* arg0)
{
  return g_object_unref(arg0);
}

void Pure_g_object_weak_ref(GObject* arg0, void* arg1, void* arg2)
{
  return g_object_weak_ref(arg0, arg1, arg2);
}

void Pure_g_object_weak_unref(GObject* arg0, void* arg1, void* arg2)
{
  return g_object_weak_unref(arg0, arg1, arg2);
}

void Pure_g_object_add_weak_pointer(GObject* arg0, void** arg1)
{
  return g_object_add_weak_pointer(arg0, arg1);
}

void Pure_g_object_remove_weak_pointer(GObject* arg0, void** arg1)
{
  return g_object_remove_weak_pointer(arg0, arg1);
}

void Pure_g_object_add_toggle_ref(GObject* arg0, void* arg1, void* arg2)
{
  return g_object_add_toggle_ref(arg0, arg1, arg2);
}

void Pure_g_object_remove_toggle_ref(GObject* arg0, void* arg1, void* arg2)
{
  return g_object_remove_toggle_ref(arg0, arg1, arg2);
}

void* Pure_g_object_get_qdata(GObject* arg0, unsigned int arg1)
{
  return g_object_get_qdata(arg0, arg1);
}

void Pure_g_object_set_qdata(GObject* arg0, unsigned int arg1, void* arg2)
{
  return g_object_set_qdata(arg0, arg1, arg2);
}

void Pure_g_object_set_qdata_full(GObject* arg0, unsigned int arg1, void* arg2, void* arg3)
{
  return g_object_set_qdata_full(arg0, arg1, arg2, arg3);
}

void* Pure_g_object_steal_qdata(GObject* arg0, unsigned int arg1)
{
  return g_object_steal_qdata(arg0, arg1);
}

void* Pure_g_object_get_data(GObject* arg0, char const* arg1)
{
  return g_object_get_data(arg0, arg1);
}

void Pure_g_object_set_data(GObject* arg0, char const* arg1, void* arg2)
{
  return g_object_set_data(arg0, arg1, arg2);
}

void Pure_g_object_set_data_full(GObject* arg0, char const* arg1, void* arg2, void* arg3)
{
  return g_object_set_data_full(arg0, arg1, arg2, arg3);
}

void* Pure_g_object_steal_data(GObject* arg0, char const* arg1)
{
  return g_object_steal_data(arg0, arg1);
}

void Pure_g_object_watch_closure(GObject* arg0, GClosure* arg1)
{
  return g_object_watch_closure(arg0, arg1);
}

GClosure* Pure_g_cclosure_new_object(void* arg0, GObject* arg1)
{
  return g_cclosure_new_object(arg0, arg1);
}

GClosure* Pure_g_cclosure_new_object_swap(void* arg0, GObject* arg1)
{
  return g_cclosure_new_object_swap(arg0, arg1);
}

GClosure* Pure_g_closure_new_object(unsigned int arg0, GObject* arg1)
{
  return g_closure_new_object(arg0, arg1);
}

void Pure_g_value_set_object(GValue* arg0, void* arg1)
{
  return g_value_set_object(arg0, arg1);
}

void* Pure_g_value_get_object(GValue const* arg0)
{
  return g_value_get_object(arg0);
}

void* Pure_g_value_dup_object(GValue const* arg0)
{
  return g_value_dup_object(arg0);
}

unsigned long Pure_g_signal_connect_object(void* arg0, char const* arg1, void* arg2, void* arg3, unsigned int arg4)
{
  return g_signal_connect_object(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_object_force_floating(GObject* arg0)
{
  return g_object_force_floating(arg0);
}

void Pure_g_object_run_dispose(GObject* arg0)
{
  return g_object_run_dispose(arg0);
}

void Pure_g_value_take_object(GValue* arg0, void* arg1)
{
  return g_value_take_object(arg0, arg1);
}

void Pure_g_value_set_object_take_ownership(GValue* arg0, void* arg1)
{
  return g_value_set_object_take_ownership(arg0, arg1);
}

unsigned long Pure_g_object_compat_control(unsigned long arg0, void* arg1)
{
  return g_object_compat_control(arg0, arg1);
}

GParamSpec* Pure_g_param_spec_char(char const* arg0, char const* arg1, char const* arg2, char arg3, char arg4, char arg5, unsigned int arg6)
{
  return g_param_spec_char(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GParamSpec* Pure_g_param_spec_uchar(char const* arg0, char const* arg1, char const* arg2, unsigned char arg3, unsigned char arg4, unsigned char arg5, unsigned int arg6)
{
  return g_param_spec_uchar(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GParamSpec* Pure_g_param_spec_boolean(char const* arg0, char const* arg1, char const* arg2, int arg3, unsigned int arg4)
{
  return g_param_spec_boolean(arg0, arg1, arg2, arg3, arg4);
}

GParamSpec* Pure_g_param_spec_int(char const* arg0, char const* arg1, char const* arg2, int arg3, int arg4, int arg5, unsigned int arg6)
{
  return g_param_spec_int(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GParamSpec* Pure_g_param_spec_uint(char const* arg0, char const* arg1, char const* arg2, unsigned int arg3, unsigned int arg4, unsigned int arg5, unsigned int arg6)
{
  return g_param_spec_uint(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GParamSpec* Pure_g_param_spec_long(char const* arg0, char const* arg1, char const* arg2, long arg3, long arg4, long arg5, unsigned int arg6)
{
  return g_param_spec_long(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GParamSpec* Pure_g_param_spec_ulong(char const* arg0, char const* arg1, char const* arg2, unsigned long arg3, unsigned long arg4, unsigned long arg5, unsigned int arg6)
{
  return g_param_spec_ulong(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GParamSpec* Pure_g_param_spec_int64(char const* arg0, char const* arg1, char const* arg2, long arg3, long arg4, long arg5, unsigned int arg6)
{
  return g_param_spec_int64(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GParamSpec* Pure_g_param_spec_uint64(char const* arg0, char const* arg1, char const* arg2, unsigned long arg3, unsigned long arg4, unsigned long arg5, unsigned int arg6)
{
  return g_param_spec_uint64(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GParamSpec* Pure_g_param_spec_unichar(char const* arg0, char const* arg1, char const* arg2, unsigned int arg3, unsigned int arg4)
{
  return g_param_spec_unichar(arg0, arg1, arg2, arg3, arg4);
}

GParamSpec* Pure_g_param_spec_enum(char const* arg0, char const* arg1, char const* arg2, unsigned long arg3, int arg4, unsigned int arg5)
{
  return g_param_spec_enum(arg0, arg1, arg2, arg3, arg4, arg5);
}

GParamSpec* Pure_g_param_spec_flags(char const* arg0, char const* arg1, char const* arg2, unsigned long arg3, unsigned int arg4, unsigned int arg5)
{
  return g_param_spec_flags(arg0, arg1, arg2, arg3, arg4, arg5);
}

GParamSpec* Pure_g_param_spec_float(char const* arg0, char const* arg1, char const* arg2, float arg3, float arg4, float arg5, unsigned int arg6)
{
  return g_param_spec_float(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GParamSpec* Pure_g_param_spec_double(char const* arg0, char const* arg1, char const* arg2, double arg3, double arg4, double arg5, unsigned int arg6)
{
  return g_param_spec_double(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GParamSpec* Pure_g_param_spec_string(char const* arg0, char const* arg1, char const* arg2, char const* arg3, unsigned int arg4)
{
  return g_param_spec_string(arg0, arg1, arg2, arg3, arg4);
}

GParamSpec* Pure_g_param_spec_param(char const* arg0, char const* arg1, char const* arg2, unsigned long arg3, unsigned int arg4)
{
  return g_param_spec_param(arg0, arg1, arg2, arg3, arg4);
}

GParamSpec* Pure_g_param_spec_boxed(char const* arg0, char const* arg1, char const* arg2, unsigned long arg3, unsigned int arg4)
{
  return g_param_spec_boxed(arg0, arg1, arg2, arg3, arg4);
}

GParamSpec* Pure_g_param_spec_pointer(char const* arg0, char const* arg1, char const* arg2, unsigned int arg3)
{
  return g_param_spec_pointer(arg0, arg1, arg2, arg3);
}

GParamSpec* Pure_g_param_spec_value_array(char const* arg0, char const* arg1, char const* arg2, GParamSpec* arg3, unsigned int arg4)
{
  return g_param_spec_value_array(arg0, arg1, arg2, arg3, arg4);
}

GParamSpec* Pure_g_param_spec_object(char const* arg0, char const* arg1, char const* arg2, unsigned long arg3, unsigned int arg4)
{
  return g_param_spec_object(arg0, arg1, arg2, arg3, arg4);
}

GParamSpec* Pure_g_param_spec_override(char const* arg0, GParamSpec* arg1)
{
  return g_param_spec_override(arg0, arg1);
}

GParamSpec* Pure_g_param_spec_gtype(char const* arg0, char const* arg1, char const* arg2, unsigned long arg3, unsigned int arg4)
{
  return g_param_spec_gtype(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_source_set_closure(GSource* arg0, GClosure* arg1)
{
  return g_source_set_closure(arg0, arg1);
}

unsigned long Pure_g_io_channel_get_type()
{
  return g_io_channel_get_type();
}

unsigned long Pure_g_io_condition_get_type()
{
  return g_io_condition_get_type();
}

unsigned long Pure_g_type_module_get_type()
{
  return g_type_module_get_type();
}

int Pure_g_type_module_use(GTypeModule* arg0)
{
  return g_type_module_use(arg0);
}

void Pure_g_type_module_unuse(GTypeModule* arg0)
{
  return g_type_module_unuse(arg0);
}

void Pure_g_type_module_set_name(GTypeModule* arg0, char const* arg1)
{
  return g_type_module_set_name(arg0, arg1);
}

unsigned long Pure_g_type_module_register_type(GTypeModule* arg0, unsigned long arg1, char const* arg2, GTypeInfo const* arg3, unsigned int arg4)
{
  return g_type_module_register_type(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_type_module_add_interface(GTypeModule* arg0, unsigned long arg1, unsigned long arg2, GInterfaceInfo const* arg3)
{
  return g_type_module_add_interface(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_type_module_register_enum(GTypeModule* arg0, char const* arg1, GEnumValue const* arg2)
{
  return g_type_module_register_enum(arg0, arg1, arg2);
}

unsigned long Pure_g_type_module_register_flags(GTypeModule* arg0, char const* arg1, GFlagsValue const* arg2)
{
  return g_type_module_register_flags(arg0, arg1, arg2);
}

unsigned long Pure_g_type_plugin_get_type()
{
  return g_type_plugin_get_type();
}

void Pure_g_type_plugin_use(GTypePlugin* arg0)
{
  return g_type_plugin_use(arg0);
}

void Pure_g_type_plugin_unuse(GTypePlugin* arg0)
{
  return g_type_plugin_unuse(arg0);
}

void Pure_g_type_plugin_complete_type_info(GTypePlugin* arg0, unsigned long arg1, GTypeInfo* arg2, GTypeValueTable* arg3)
{
  return g_type_plugin_complete_type_info(arg0, arg1, arg2, arg3);
}

void Pure_g_type_plugin_complete_interface_info(GTypePlugin* arg0, unsigned long arg1, unsigned long arg2, GInterfaceInfo* arg3)
{
  return g_type_plugin_complete_interface_info(arg0, arg1, arg2, arg3);
}

GValue* Pure_g_value_array_get_nth(GValueArray* arg0, unsigned int arg1)
{
  return g_value_array_get_nth(arg0, arg1);
}

GValueArray* Pure_g_value_array_new(unsigned int arg0)
{
  return g_value_array_new(arg0);
}

void Pure_g_value_array_free(GValueArray* arg0)
{
  return g_value_array_free(arg0);
}

GValueArray* Pure_g_value_array_copy(GValueArray const* arg0)
{
  return g_value_array_copy(arg0);
}

GValueArray* Pure_g_value_array_prepend(GValueArray* arg0, GValue const* arg1)
{
  return g_value_array_prepend(arg0, arg1);
}

GValueArray* Pure_g_value_array_append(GValueArray* arg0, GValue const* arg1)
{
  return g_value_array_append(arg0, arg1);
}

GValueArray* Pure_g_value_array_insert(GValueArray* arg0, unsigned int arg1, GValue const* arg2)
{
  return g_value_array_insert(arg0, arg1, arg2);
}

GValueArray* Pure_g_value_array_remove(GValueArray* arg0, unsigned int arg1)
{
  return g_value_array_remove(arg0, arg1);
}

GValueArray* Pure_g_value_array_sort(GValueArray* arg0, void* arg1)
{
  return g_value_array_sort(arg0, arg1);
}

GValueArray* Pure_g_value_array_sort_with_data(GValueArray* arg0, void* arg1, void* arg2)
{
  return g_value_array_sort_with_data(arg0, arg1, arg2);
}

void Pure_g_value_set_char(GValue* arg0, char arg1)
{
  return g_value_set_char(arg0, arg1);
}

char Pure_g_value_get_char(GValue const* arg0)
{
  return g_value_get_char(arg0);
}

void Pure_g_value_set_uchar(GValue* arg0, unsigned char arg1)
{
  return g_value_set_uchar(arg0, arg1);
}

unsigned char Pure_g_value_get_uchar(GValue const* arg0)
{
  return g_value_get_uchar(arg0);
}

void Pure_g_value_set_boolean(GValue* arg0, int arg1)
{
  return g_value_set_boolean(arg0, arg1);
}

int Pure_g_value_get_boolean(GValue const* arg0)
{
  return g_value_get_boolean(arg0);
}

void Pure_g_value_set_int(GValue* arg0, int arg1)
{
  return g_value_set_int(arg0, arg1);
}

int Pure_g_value_get_int(GValue const* arg0)
{
  return g_value_get_int(arg0);
}

void Pure_g_value_set_uint(GValue* arg0, unsigned int arg1)
{
  return g_value_set_uint(arg0, arg1);
}

unsigned int Pure_g_value_get_uint(GValue const* arg0)
{
  return g_value_get_uint(arg0);
}

void Pure_g_value_set_long(GValue* arg0, long arg1)
{
  return g_value_set_long(arg0, arg1);
}

long Pure_g_value_get_long(GValue const* arg0)
{
  return g_value_get_long(arg0);
}

void Pure_g_value_set_ulong(GValue* arg0, unsigned long arg1)
{
  return g_value_set_ulong(arg0, arg1);
}

unsigned long Pure_g_value_get_ulong(GValue const* arg0)
{
  return g_value_get_ulong(arg0);
}

void Pure_g_value_set_int64(GValue* arg0, long arg1)
{
  return g_value_set_int64(arg0, arg1);
}

long Pure_g_value_get_int64(GValue const* arg0)
{
  return g_value_get_int64(arg0);
}

void Pure_g_value_set_uint64(GValue* arg0, unsigned long arg1)
{
  return g_value_set_uint64(arg0, arg1);
}

unsigned long Pure_g_value_get_uint64(GValue const* arg0)
{
  return g_value_get_uint64(arg0);
}

void Pure_g_value_set_float(GValue* arg0, float arg1)
{
  return g_value_set_float(arg0, arg1);
}

float Pure_g_value_get_float(GValue const* arg0)
{
  return g_value_get_float(arg0);
}

void Pure_g_value_set_double(GValue* arg0, double arg1)
{
  return g_value_set_double(arg0, arg1);
}

double Pure_g_value_get_double(GValue const* arg0)
{
  return g_value_get_double(arg0);
}

void Pure_g_value_set_string(GValue* arg0, char const* arg1)
{
  return g_value_set_string(arg0, arg1);
}

void Pure_g_value_set_static_string(GValue* arg0, char const* arg1)
{
  return g_value_set_static_string(arg0, arg1);
}

char const* Pure_g_value_get_string(GValue const* arg0)
{
  return g_value_get_string(arg0);
}

char* Pure_g_value_dup_string(GValue const* arg0)
{
  return g_value_dup_string(arg0);
}

void Pure_g_value_set_pointer(GValue* arg0, void* arg1)
{
  return g_value_set_pointer(arg0, arg1);
}

void* Pure_g_value_get_pointer(GValue const* arg0)
{
  return g_value_get_pointer(arg0);
}

unsigned long Pure_g_gtype_get_type()
{
  return g_gtype_get_type();
}

void Pure_g_value_set_gtype(GValue* arg0, unsigned long arg1)
{
  return g_value_set_gtype(arg0, arg1);
}

unsigned long Pure_g_value_get_gtype(GValue const* arg0)
{
  return g_value_get_gtype(arg0);
}

unsigned long Pure_g_pointer_type_register_static(char const* arg0)
{
  return g_pointer_type_register_static(arg0);
}

char* Pure_g_strdup_value_contents(GValue const* arg0)
{
  return g_strdup_value_contents(arg0);
}

void Pure_g_value_take_string(GValue* arg0, char* arg1)
{
  return g_value_take_string(arg0, arg1);
}

void Pure_g_value_set_string_take_ownership(GValue* arg0, char* arg1)
{
  return g_value_set_string_take_ownership(arg0, arg1);
}
#include <gio/gio.h>

unsigned long Pure_g_app_info_get_type()
{
  return g_app_info_get_type();
}

GAppInfo* Pure_g_app_info_create_from_commandline(char const* arg0, char const* arg1, unsigned int arg2, GError** arg3)
{
  return g_app_info_create_from_commandline(arg0, arg1, arg2, arg3);
}

GAppInfo* Pure_g_app_info_dup(GAppInfo* arg0)
{
  return g_app_info_dup(arg0);
}

int Pure_g_app_info_equal(GAppInfo* arg0, GAppInfo* arg1)
{
  return g_app_info_equal(arg0, arg1);
}

char const* Pure_g_app_info_get_id(GAppInfo* arg0)
{
  return g_app_info_get_id(arg0);
}

char const* Pure_g_app_info_get_name(GAppInfo* arg0)
{
  return g_app_info_get_name(arg0);
}

char const* Pure_g_app_info_get_description(GAppInfo* arg0)
{
  return g_app_info_get_description(arg0);
}

char const* Pure_g_app_info_get_executable(GAppInfo* arg0)
{
  return g_app_info_get_executable(arg0);
}

GIcon* Pure_g_app_info_get_icon(GAppInfo* arg0)
{
  return g_app_info_get_icon(arg0);
}

int Pure_g_app_info_launch(GAppInfo* arg0, GList* arg1, GAppLaunchContext* arg2, GError** arg3)
{
  return g_app_info_launch(arg0, arg1, arg2, arg3);
}

int Pure_g_app_info_supports_uris(GAppInfo* arg0)
{
  return g_app_info_supports_uris(arg0);
}

int Pure_g_app_info_supports_files(GAppInfo* arg0)
{
  return g_app_info_supports_files(arg0);
}

int Pure_g_app_info_launch_uris(GAppInfo* arg0, GList* arg1, GAppLaunchContext* arg2, GError** arg3)
{
  return g_app_info_launch_uris(arg0, arg1, arg2, arg3);
}

int Pure_g_app_info_should_show(GAppInfo* arg0)
{
  return g_app_info_should_show(arg0);
}

int Pure_g_app_info_set_as_default_for_type(GAppInfo* arg0, char const* arg1, GError** arg2)
{
  return g_app_info_set_as_default_for_type(arg0, arg1, arg2);
}

int Pure_g_app_info_set_as_default_for_extension(GAppInfo* arg0, char const* arg1, GError** arg2)
{
  return g_app_info_set_as_default_for_extension(arg0, arg1, arg2);
}

int Pure_g_app_info_add_supports_type(GAppInfo* arg0, char const* arg1, GError** arg2)
{
  return g_app_info_add_supports_type(arg0, arg1, arg2);
}

int Pure_g_app_info_can_remove_supports_type(GAppInfo* arg0)
{
  return g_app_info_can_remove_supports_type(arg0);
}

int Pure_g_app_info_remove_supports_type(GAppInfo* arg0, char const* arg1, GError** arg2)
{
  return g_app_info_remove_supports_type(arg0, arg1, arg2);
}

GList* Pure_g_app_info_get_all()
{
  return g_app_info_get_all();
}

GList* Pure_g_app_info_get_all_for_type(char const* arg0)
{
  return g_app_info_get_all_for_type(arg0);
}

GAppInfo* Pure_g_app_info_get_default_for_type(char const* arg0, int arg1)
{
  return g_app_info_get_default_for_type(arg0, arg1);
}

GAppInfo* Pure_g_app_info_get_default_for_uri_scheme(char const* arg0)
{
  return g_app_info_get_default_for_uri_scheme(arg0);
}

int Pure_g_app_info_launch_default_for_uri(char const* arg0, GAppLaunchContext* arg1, GError** arg2)
{
  return g_app_info_launch_default_for_uri(arg0, arg1, arg2);
}

unsigned long Pure_g_app_launch_context_get_type()
{
  return g_app_launch_context_get_type();
}

GAppLaunchContext* Pure_g_app_launch_context_new()
{
  return g_app_launch_context_new();
}

char* Pure_g_app_launch_context_get_display(GAppLaunchContext* arg0, GAppInfo* arg1, GList* arg2)
{
  return g_app_launch_context_get_display(arg0, arg1, arg2);
}

char* Pure_g_app_launch_context_get_startup_notify_id(GAppLaunchContext* arg0, GAppInfo* arg1, GList* arg2)
{
  return g_app_launch_context_get_startup_notify_id(arg0, arg1, arg2);
}

void Pure_g_app_launch_context_launch_failed(GAppLaunchContext* arg0, char const* arg1)
{
  return g_app_launch_context_launch_failed(arg0, arg1);
}

unsigned long Pure_g_async_result_get_type()
{
  return g_async_result_get_type();
}

void* Pure_g_async_result_get_user_data(GAsyncResult* arg0)
{
  return g_async_result_get_user_data(arg0);
}

GObject* Pure_g_async_result_get_source_object(GAsyncResult* arg0)
{
  return g_async_result_get_source_object(arg0);
}

unsigned long Pure_g_input_stream_get_type()
{
  return g_input_stream_get_type();
}

long Pure_g_input_stream_read(GInputStream* arg0, void* arg1, unsigned long arg2, GCancellable* arg3, GError** arg4)
{
  return g_input_stream_read(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_input_stream_read_all(GInputStream* arg0, void* arg1, unsigned long arg2, unsigned long* arg3, GCancellable* arg4, GError** arg5)
{
  return g_input_stream_read_all(arg0, arg1, arg2, arg3, arg4, arg5);
}

long Pure_g_input_stream_skip(GInputStream* arg0, unsigned long arg1, GCancellable* arg2, GError** arg3)
{
  return g_input_stream_skip(arg0, arg1, arg2, arg3);
}

int Pure_g_input_stream_close(GInputStream* arg0, GCancellable* arg1, GError** arg2)
{
  return g_input_stream_close(arg0, arg1, arg2);
}

void Pure_g_input_stream_read_async(GInputStream* arg0, void* arg1, unsigned long arg2, int arg3, GCancellable* arg4, void* arg5, void* arg6)
{
  return g_input_stream_read_async(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

long Pure_g_input_stream_read_finish(GInputStream* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_input_stream_read_finish(arg0, arg1, arg2);
}

void Pure_g_input_stream_skip_async(GInputStream* arg0, unsigned long arg1, int arg2, GCancellable* arg3, void* arg4, void* arg5)
{
  return g_input_stream_skip_async(arg0, arg1, arg2, arg3, arg4, arg5);
}

long Pure_g_input_stream_skip_finish(GInputStream* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_input_stream_skip_finish(arg0, arg1, arg2);
}

void Pure_g_input_stream_close_async(GInputStream* arg0, int arg1, GCancellable* arg2, void* arg3, void* arg4)
{
  return g_input_stream_close_async(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_input_stream_close_finish(GInputStream* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_input_stream_close_finish(arg0, arg1, arg2);
}

int Pure_g_input_stream_is_closed(GInputStream* arg0)
{
  return g_input_stream_is_closed(arg0);
}

int Pure_g_input_stream_has_pending(GInputStream* arg0)
{
  return g_input_stream_has_pending(arg0);
}

int Pure_g_input_stream_set_pending(GInputStream* arg0, GError** arg1)
{
  return g_input_stream_set_pending(arg0, arg1);
}

void Pure_g_input_stream_clear_pending(GInputStream* arg0)
{
  return g_input_stream_clear_pending(arg0);
}

unsigned long Pure_g_filter_input_stream_get_type()
{
  return g_filter_input_stream_get_type();
}

GInputStream* Pure_g_filter_input_stream_get_base_stream(GFilterInputStream* arg0)
{
  return g_filter_input_stream_get_base_stream(arg0);
}

unsigned long Pure_g_buffered_input_stream_get_type()
{
  return g_buffered_input_stream_get_type();
}

GInputStream* Pure_g_buffered_input_stream_new(GInputStream* arg0)
{
  return g_buffered_input_stream_new(arg0);
}

GInputStream* Pure_g_buffered_input_stream_new_sized(GInputStream* arg0, unsigned long arg1)
{
  return g_buffered_input_stream_new_sized(arg0, arg1);
}

unsigned long Pure_g_buffered_input_stream_get_buffer_size(GBufferedInputStream* arg0)
{
  return g_buffered_input_stream_get_buffer_size(arg0);
}

void Pure_g_buffered_input_stream_set_buffer_size(GBufferedInputStream* arg0, unsigned long arg1)
{
  return g_buffered_input_stream_set_buffer_size(arg0, arg1);
}

unsigned long Pure_g_buffered_input_stream_get_available(GBufferedInputStream* arg0)
{
  return g_buffered_input_stream_get_available(arg0);
}

unsigned long Pure_g_buffered_input_stream_peek(GBufferedInputStream* arg0, void* arg1, unsigned long arg2, unsigned long arg3)
{
  return g_buffered_input_stream_peek(arg0, arg1, arg2, arg3);
}

void const* Pure_g_buffered_input_stream_peek_buffer(GBufferedInputStream* arg0, unsigned long* arg1)
{
  return g_buffered_input_stream_peek_buffer(arg0, arg1);
}

long Pure_g_buffered_input_stream_fill(GBufferedInputStream* arg0, long arg1, GCancellable* arg2, GError** arg3)
{
  return g_buffered_input_stream_fill(arg0, arg1, arg2, arg3);
}

void Pure_g_buffered_input_stream_fill_async(GBufferedInputStream* arg0, long arg1, int arg2, GCancellable* arg3, void* arg4, void* arg5)
{
  return g_buffered_input_stream_fill_async(arg0, arg1, arg2, arg3, arg4, arg5);
}

long Pure_g_buffered_input_stream_fill_finish(GBufferedInputStream* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_buffered_input_stream_fill_finish(arg0, arg1, arg2);
}

int Pure_g_buffered_input_stream_read_byte(GBufferedInputStream* arg0, GCancellable* arg1, GError** arg2)
{
  return g_buffered_input_stream_read_byte(arg0, arg1, arg2);
}

unsigned long Pure_g_output_stream_get_type()
{
  return g_output_stream_get_type();
}

long Pure_g_output_stream_write(GOutputStream* arg0, void const* arg1, unsigned long arg2, GCancellable* arg3, GError** arg4)
{
  return g_output_stream_write(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_output_stream_write_all(GOutputStream* arg0, void const* arg1, unsigned long arg2, unsigned long* arg3, GCancellable* arg4, GError** arg5)
{
  return g_output_stream_write_all(arg0, arg1, arg2, arg3, arg4, arg5);
}

long Pure_g_output_stream_splice(GOutputStream* arg0, GInputStream* arg1, unsigned int arg2, GCancellable* arg3, GError** arg4)
{
  return g_output_stream_splice(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_output_stream_flush(GOutputStream* arg0, GCancellable* arg1, GError** arg2)
{
  return g_output_stream_flush(arg0, arg1, arg2);
}

int Pure_g_output_stream_close(GOutputStream* arg0, GCancellable* arg1, GError** arg2)
{
  return g_output_stream_close(arg0, arg1, arg2);
}

void Pure_g_output_stream_write_async(GOutputStream* arg0, void const* arg1, unsigned long arg2, int arg3, GCancellable* arg4, void* arg5, void* arg6)
{
  return g_output_stream_write_async(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

long Pure_g_output_stream_write_finish(GOutputStream* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_output_stream_write_finish(arg0, arg1, arg2);
}

void Pure_g_output_stream_splice_async(GOutputStream* arg0, GInputStream* arg1, unsigned int arg2, int arg3, GCancellable* arg4, void* arg5, void* arg6)
{
  return g_output_stream_splice_async(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

long Pure_g_output_stream_splice_finish(GOutputStream* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_output_stream_splice_finish(arg0, arg1, arg2);
}

void Pure_g_output_stream_flush_async(GOutputStream* arg0, int arg1, GCancellable* arg2, void* arg3, void* arg4)
{
  return g_output_stream_flush_async(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_output_stream_flush_finish(GOutputStream* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_output_stream_flush_finish(arg0, arg1, arg2);
}

void Pure_g_output_stream_close_async(GOutputStream* arg0, int arg1, GCancellable* arg2, void* arg3, void* arg4)
{
  return g_output_stream_close_async(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_output_stream_close_finish(GOutputStream* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_output_stream_close_finish(arg0, arg1, arg2);
}

int Pure_g_output_stream_is_closed(GOutputStream* arg0)
{
  return g_output_stream_is_closed(arg0);
}

int Pure_g_output_stream_has_pending(GOutputStream* arg0)
{
  return g_output_stream_has_pending(arg0);
}

int Pure_g_output_stream_set_pending(GOutputStream* arg0, GError** arg1)
{
  return g_output_stream_set_pending(arg0, arg1);
}

void Pure_g_output_stream_clear_pending(GOutputStream* arg0)
{
  return g_output_stream_clear_pending(arg0);
}

unsigned long Pure_g_filter_output_stream_get_type()
{
  return g_filter_output_stream_get_type();
}

GOutputStream* Pure_g_filter_output_stream_get_base_stream(GFilterOutputStream* arg0)
{
  return g_filter_output_stream_get_base_stream(arg0);
}

unsigned long Pure_g_buffered_output_stream_get_type()
{
  return g_buffered_output_stream_get_type();
}

GOutputStream* Pure_g_buffered_output_stream_new(GOutputStream* arg0)
{
  return g_buffered_output_stream_new(arg0);
}

GOutputStream* Pure_g_buffered_output_stream_new_sized(GOutputStream* arg0, unsigned long arg1)
{
  return g_buffered_output_stream_new_sized(arg0, arg1);
}

unsigned long Pure_g_buffered_output_stream_get_buffer_size(GBufferedOutputStream* arg0)
{
  return g_buffered_output_stream_get_buffer_size(arg0);
}

void Pure_g_buffered_output_stream_set_buffer_size(GBufferedOutputStream* arg0, unsigned long arg1)
{
  return g_buffered_output_stream_set_buffer_size(arg0, arg1);
}

int Pure_g_buffered_output_stream_get_auto_grow(GBufferedOutputStream* arg0)
{
  return g_buffered_output_stream_get_auto_grow(arg0);
}

void Pure_g_buffered_output_stream_set_auto_grow(GBufferedOutputStream* arg0, int arg1)
{
  return g_buffered_output_stream_set_auto_grow(arg0, arg1);
}

unsigned long Pure_g_cancellable_get_type()
{
  return g_cancellable_get_type();
}

GCancellable* Pure_g_cancellable_new()
{
  return g_cancellable_new();
}

int Pure_g_cancellable_is_cancelled(GCancellable* arg0)
{
  return g_cancellable_is_cancelled(arg0);
}

int Pure_g_cancellable_set_error_if_cancelled(GCancellable* arg0, GError** arg1)
{
  return g_cancellable_set_error_if_cancelled(arg0, arg1);
}

int Pure_g_cancellable_get_fd(GCancellable* arg0)
{
  return g_cancellable_get_fd(arg0);
}

GCancellable* Pure_g_cancellable_get_current()
{
  return g_cancellable_get_current();
}

void Pure_g_cancellable_push_current(GCancellable* arg0)
{
  return g_cancellable_push_current(arg0);
}

void Pure_g_cancellable_pop_current(GCancellable* arg0)
{
  return g_cancellable_pop_current(arg0);
}

void Pure_g_cancellable_reset(GCancellable* arg0)
{
  return g_cancellable_reset(arg0);
}

void Pure_g_cancellable_cancel(GCancellable* arg0)
{
  return g_cancellable_cancel(arg0);
}

int Pure_g_content_type_equals(char const* arg0, char const* arg1)
{
  return g_content_type_equals(arg0, arg1);
}

int Pure_g_content_type_is_a(char const* arg0, char const* arg1)
{
  return g_content_type_is_a(arg0, arg1);
}

int Pure_g_content_type_is_unknown(char const* arg0)
{
  return g_content_type_is_unknown(arg0);
}

char* Pure_g_content_type_get_description(char const* arg0)
{
  return g_content_type_get_description(arg0);
}

char* Pure_g_content_type_get_mime_type(char const* arg0)
{
  return g_content_type_get_mime_type(arg0);
}

GIcon* Pure_g_content_type_get_icon(char const* arg0)
{
  return g_content_type_get_icon(arg0);
}

int Pure_g_content_type_can_be_executable(char const* arg0)
{
  return g_content_type_can_be_executable(arg0);
}

char* Pure_g_content_type_from_mime_type(char const* arg0)
{
  return g_content_type_from_mime_type(arg0);
}

char* Pure_g_content_type_guess(char const* arg0, unsigned char const* arg1, unsigned long arg2, int* arg3)
{
  return g_content_type_guess(arg0, arg1, arg2, arg3);
}

char** Pure_g_content_type_guess_for_tree(GFile* arg0)
{
  return g_content_type_guess_for_tree(arg0);
}

GList* Pure_g_content_types_get_registered()
{
  return g_content_types_get_registered();
}

unsigned long Pure_g_data_input_stream_get_type()
{
  return g_data_input_stream_get_type();
}

GDataInputStream* Pure_g_data_input_stream_new(GInputStream* arg0)
{
  return g_data_input_stream_new(arg0);
}

void Pure_g_data_input_stream_set_byte_order(GDataInputStream* arg0, unsigned int arg1)
{
  return g_data_input_stream_set_byte_order(arg0, arg1);
}

unsigned int Pure_g_data_input_stream_get_byte_order(GDataInputStream* arg0)
{
  return g_data_input_stream_get_byte_order(arg0);
}

void Pure_g_data_input_stream_set_newline_type(GDataInputStream* arg0, unsigned int arg1)
{
  return g_data_input_stream_set_newline_type(arg0, arg1);
}

unsigned int Pure_g_data_input_stream_get_newline_type(GDataInputStream* arg0)
{
  return g_data_input_stream_get_newline_type(arg0);
}

unsigned char Pure_g_data_input_stream_read_byte(GDataInputStream* arg0, GCancellable* arg1, GError** arg2)
{
  return g_data_input_stream_read_byte(arg0, arg1, arg2);
}

short Pure_g_data_input_stream_read_int16(GDataInputStream* arg0, GCancellable* arg1, GError** arg2)
{
  return g_data_input_stream_read_int16(arg0, arg1, arg2);
}

unsigned short Pure_g_data_input_stream_read_uint16(GDataInputStream* arg0, GCancellable* arg1, GError** arg2)
{
  return g_data_input_stream_read_uint16(arg0, arg1, arg2);
}

int Pure_g_data_input_stream_read_int32(GDataInputStream* arg0, GCancellable* arg1, GError** arg2)
{
  return g_data_input_stream_read_int32(arg0, arg1, arg2);
}

unsigned int Pure_g_data_input_stream_read_uint32(GDataInputStream* arg0, GCancellable* arg1, GError** arg2)
{
  return g_data_input_stream_read_uint32(arg0, arg1, arg2);
}

long Pure_g_data_input_stream_read_int64(GDataInputStream* arg0, GCancellable* arg1, GError** arg2)
{
  return g_data_input_stream_read_int64(arg0, arg1, arg2);
}

unsigned long Pure_g_data_input_stream_read_uint64(GDataInputStream* arg0, GCancellable* arg1, GError** arg2)
{
  return g_data_input_stream_read_uint64(arg0, arg1, arg2);
}

char* Pure_g_data_input_stream_read_line(GDataInputStream* arg0, unsigned long* arg1, GCancellable* arg2, GError** arg3)
{
  return g_data_input_stream_read_line(arg0, arg1, arg2, arg3);
}

char* Pure_g_data_input_stream_read_until(GDataInputStream* arg0, char const* arg1, unsigned long* arg2, GCancellable* arg3, GError** arg4)
{
  return g_data_input_stream_read_until(arg0, arg1, arg2, arg3, arg4);
}

unsigned long Pure_g_data_output_stream_get_type()
{
  return g_data_output_stream_get_type();
}

GDataOutputStream* Pure_g_data_output_stream_new(GOutputStream* arg0)
{
  return g_data_output_stream_new(arg0);
}

void Pure_g_data_output_stream_set_byte_order(GDataOutputStream* arg0, unsigned int arg1)
{
  return g_data_output_stream_set_byte_order(arg0, arg1);
}

unsigned int Pure_g_data_output_stream_get_byte_order(GDataOutputStream* arg0)
{
  return g_data_output_stream_get_byte_order(arg0);
}

int Pure_g_data_output_stream_put_byte(GDataOutputStream* arg0, unsigned char arg1, GCancellable* arg2, GError** arg3)
{
  return g_data_output_stream_put_byte(arg0, arg1, arg2, arg3);
}

int Pure_g_data_output_stream_put_int16(GDataOutputStream* arg0, short arg1, GCancellable* arg2, GError** arg3)
{
  return g_data_output_stream_put_int16(arg0, arg1, arg2, arg3);
}

int Pure_g_data_output_stream_put_uint16(GDataOutputStream* arg0, unsigned short arg1, GCancellable* arg2, GError** arg3)
{
  return g_data_output_stream_put_uint16(arg0, arg1, arg2, arg3);
}

int Pure_g_data_output_stream_put_int32(GDataOutputStream* arg0, int arg1, GCancellable* arg2, GError** arg3)
{
  return g_data_output_stream_put_int32(arg0, arg1, arg2, arg3);
}

int Pure_g_data_output_stream_put_uint32(GDataOutputStream* arg0, unsigned int arg1, GCancellable* arg2, GError** arg3)
{
  return g_data_output_stream_put_uint32(arg0, arg1, arg2, arg3);
}

int Pure_g_data_output_stream_put_int64(GDataOutputStream* arg0, long arg1, GCancellable* arg2, GError** arg3)
{
  return g_data_output_stream_put_int64(arg0, arg1, arg2, arg3);
}

int Pure_g_data_output_stream_put_uint64(GDataOutputStream* arg0, unsigned long arg1, GCancellable* arg2, GError** arg3)
{
  return g_data_output_stream_put_uint64(arg0, arg1, arg2, arg3);
}

int Pure_g_data_output_stream_put_string(GDataOutputStream* arg0, char const* arg1, GCancellable* arg2, GError** arg3)
{
  return g_data_output_stream_put_string(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_drive_get_type()
{
  return g_drive_get_type();
}

char* Pure_g_drive_get_name(GDrive* arg0)
{
  return g_drive_get_name(arg0);
}

GIcon* Pure_g_drive_get_icon(GDrive* arg0)
{
  return g_drive_get_icon(arg0);
}

int Pure_g_drive_has_volumes(GDrive* arg0)
{
  return g_drive_has_volumes(arg0);
}

GList* Pure_g_drive_get_volumes(GDrive* arg0)
{
  return g_drive_get_volumes(arg0);
}

int Pure_g_drive_is_media_removable(GDrive* arg0)
{
  return g_drive_is_media_removable(arg0);
}

int Pure_g_drive_has_media(GDrive* arg0)
{
  return g_drive_has_media(arg0);
}

int Pure_g_drive_is_media_check_automatic(GDrive* arg0)
{
  return g_drive_is_media_check_automatic(arg0);
}

int Pure_g_drive_can_poll_for_media(GDrive* arg0)
{
  return g_drive_can_poll_for_media(arg0);
}

int Pure_g_drive_can_eject(GDrive* arg0)
{
  return g_drive_can_eject(arg0);
}

void Pure_g_drive_eject(GDrive* arg0, unsigned int arg1, GCancellable* arg2, void* arg3, void* arg4)
{
  return g_drive_eject(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_drive_eject_finish(GDrive* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_drive_eject_finish(arg0, arg1, arg2);
}

void Pure_g_drive_poll_for_media(GDrive* arg0, GCancellable* arg1, void* arg2, void* arg3)
{
  return g_drive_poll_for_media(arg0, arg1, arg2, arg3);
}

int Pure_g_drive_poll_for_media_finish(GDrive* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_drive_poll_for_media_finish(arg0, arg1, arg2);
}

char* Pure_g_drive_get_identifier(GDrive* arg0, char const* arg1)
{
  return g_drive_get_identifier(arg0, arg1);
}

char** Pure_g_drive_enumerate_identifiers(GDrive* arg0)
{
  return g_drive_enumerate_identifiers(arg0);
}

unsigned long Pure_g_icon_get_type()
{
  return g_icon_get_type();
}

unsigned int Pure_g_icon_hash(void const* arg0)
{
  return g_icon_hash(arg0);
}

int Pure_g_icon_equal(GIcon* arg0, GIcon* arg1)
{
  return g_icon_equal(arg0, arg1);
}

unsigned long Pure_g_emblem_get_type()
{
  return g_emblem_get_type();
}

GEmblem* Pure_g_emblem_new(GIcon* arg0)
{
  return g_emblem_new(arg0);
}

GEmblem* Pure_g_emblem_new_with_origin(GIcon* arg0, unsigned int arg1)
{
  return g_emblem_new_with_origin(arg0, arg1);
}

GIcon* Pure_g_emblem_get_icon(GEmblem* arg0)
{
  return g_emblem_get_icon(arg0);
}

unsigned int Pure_g_emblem_get_origin(GEmblem* arg0)
{
  return g_emblem_get_origin(arg0);
}

unsigned long Pure_g_emblemed_icon_get_type()
{
  return g_emblemed_icon_get_type();
}

GIcon* Pure_g_emblemed_icon_new(GIcon* arg0, GEmblem* arg1)
{
  return g_emblemed_icon_new(arg0, arg1);
}

GIcon* Pure_g_emblemed_icon_get_icon(GEmblemedIcon* arg0)
{
  return g_emblemed_icon_get_icon(arg0);
}

GList* Pure_g_emblemed_icon_get_emblems(GEmblemedIcon* arg0)
{
  return g_emblemed_icon_get_emblems(arg0);
}

void Pure_g_emblemed_icon_add_emblem(GEmblemedIcon* arg0, GEmblem* arg1)
{
  return g_emblemed_icon_add_emblem(arg0, arg1);
}

unsigned long Pure_g_file_get_type()
{
  return g_file_get_type();
}

GFile* Pure_g_file_new_for_path(char const* arg0)
{
  return g_file_new_for_path(arg0);
}

GFile* Pure_g_file_new_for_uri(char const* arg0)
{
  return g_file_new_for_uri(arg0);
}

GFile* Pure_g_file_new_for_commandline_arg(char const* arg0)
{
  return g_file_new_for_commandline_arg(arg0);
}

GFile* Pure_g_file_parse_name(char const* arg0)
{
  return g_file_parse_name(arg0);
}

GFile* Pure_g_file_dup(GFile* arg0)
{
  return g_file_dup(arg0);
}

unsigned int Pure_g_file_hash(void const* arg0)
{
  return g_file_hash(arg0);
}

int Pure_g_file_equal(GFile* arg0, GFile* arg1)
{
  return g_file_equal(arg0, arg1);
}

char* Pure_g_file_get_basename(GFile* arg0)
{
  return g_file_get_basename(arg0);
}

char* Pure_g_file_get_path(GFile* arg0)
{
  return g_file_get_path(arg0);
}

char* Pure_g_file_get_uri(GFile* arg0)
{
  return g_file_get_uri(arg0);
}

char* Pure_g_file_get_parse_name(GFile* arg0)
{
  return g_file_get_parse_name(arg0);
}

GFile* Pure_g_file_get_parent(GFile* arg0)
{
  return g_file_get_parent(arg0);
}

GFile* Pure_g_file_get_child(GFile* arg0, char const* arg1)
{
  return g_file_get_child(arg0, arg1);
}

GFile* Pure_g_file_get_child_for_display_name(GFile* arg0, char const* arg1, GError** arg2)
{
  return g_file_get_child_for_display_name(arg0, arg1, arg2);
}

int Pure_g_file_has_prefix(GFile* arg0, GFile* arg1)
{
  return g_file_has_prefix(arg0, arg1);
}

char* Pure_g_file_get_relative_path(GFile* arg0, GFile* arg1)
{
  return g_file_get_relative_path(arg0, arg1);
}

GFile* Pure_g_file_resolve_relative_path(GFile* arg0, char const* arg1)
{
  return g_file_resolve_relative_path(arg0, arg1);
}

int Pure_g_file_is_native(GFile* arg0)
{
  return g_file_is_native(arg0);
}

int Pure_g_file_has_uri_scheme(GFile* arg0, char const* arg1)
{
  return g_file_has_uri_scheme(arg0, arg1);
}

char* Pure_g_file_get_uri_scheme(GFile* arg0)
{
  return g_file_get_uri_scheme(arg0);
}

GFileInputStream* Pure_g_file_read(GFile* arg0, GCancellable* arg1, GError** arg2)
{
  return g_file_read(arg0, arg1, arg2);
}

void Pure_g_file_read_async(GFile* arg0, int arg1, GCancellable* arg2, void* arg3, void* arg4)
{
  return g_file_read_async(arg0, arg1, arg2, arg3, arg4);
}

GFileInputStream* Pure_g_file_read_finish(GFile* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_read_finish(arg0, arg1, arg2);
}

GFileOutputStream* Pure_g_file_append_to(GFile* arg0, unsigned int arg1, GCancellable* arg2, GError** arg3)
{
  return g_file_append_to(arg0, arg1, arg2, arg3);
}

GFileOutputStream* Pure_g_file_create(GFile* arg0, unsigned int arg1, GCancellable* arg2, GError** arg3)
{
  return g_file_create(arg0, arg1, arg2, arg3);
}

GFileOutputStream* Pure_g_file_replace(GFile* arg0, char const* arg1, int arg2, unsigned int arg3, GCancellable* arg4, GError** arg5)
{
  return g_file_replace(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_file_append_to_async(GFile* arg0, unsigned int arg1, int arg2, GCancellable* arg3, void* arg4, void* arg5)
{
  return g_file_append_to_async(arg0, arg1, arg2, arg3, arg4, arg5);
}

GFileOutputStream* Pure_g_file_append_to_finish(GFile* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_append_to_finish(arg0, arg1, arg2);
}

void Pure_g_file_create_async(GFile* arg0, unsigned int arg1, int arg2, GCancellable* arg3, void* arg4, void* arg5)
{
  return g_file_create_async(arg0, arg1, arg2, arg3, arg4, arg5);
}

GFileOutputStream* Pure_g_file_create_finish(GFile* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_create_finish(arg0, arg1, arg2);
}

void Pure_g_file_replace_async(GFile* arg0, char const* arg1, int arg2, unsigned int arg3, int arg4, GCancellable* arg5, void* arg6, void* arg7)
{
  return g_file_replace_async(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

GFileOutputStream* Pure_g_file_replace_finish(GFile* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_replace_finish(arg0, arg1, arg2);
}

int Pure_g_file_query_exists(GFile* arg0, GCancellable* arg1)
{
  return g_file_query_exists(arg0, arg1);
}

unsigned int Pure_g_file_query_file_type(GFile* arg0, unsigned int arg1, GCancellable* arg2)
{
  return g_file_query_file_type(arg0, arg1, arg2);
}

GFileInfo* Pure_g_file_query_info(GFile* arg0, char const* arg1, unsigned int arg2, GCancellable* arg3, GError** arg4)
{
  return g_file_query_info(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_file_query_info_async(GFile* arg0, char const* arg1, unsigned int arg2, int arg3, GCancellable* arg4, void* arg5, void* arg6)
{
  return g_file_query_info_async(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GFileInfo* Pure_g_file_query_info_finish(GFile* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_query_info_finish(arg0, arg1, arg2);
}

GFileInfo* Pure_g_file_query_filesystem_info(GFile* arg0, char const* arg1, GCancellable* arg2, GError** arg3)
{
  return g_file_query_filesystem_info(arg0, arg1, arg2, arg3);
}

void Pure_g_file_query_filesystem_info_async(GFile* arg0, char const* arg1, int arg2, GCancellable* arg3, void* arg4, void* arg5)
{
  return g_file_query_filesystem_info_async(arg0, arg1, arg2, arg3, arg4, arg5);
}

GFileInfo* Pure_g_file_query_filesystem_info_finish(GFile* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_query_filesystem_info_finish(arg0, arg1, arg2);
}

GMount* Pure_g_file_find_enclosing_mount(GFile* arg0, GCancellable* arg1, GError** arg2)
{
  return g_file_find_enclosing_mount(arg0, arg1, arg2);
}

void Pure_g_file_find_enclosing_mount_async(GFile* arg0, int arg1, GCancellable* arg2, void* arg3, void* arg4)
{
  return g_file_find_enclosing_mount_async(arg0, arg1, arg2, arg3, arg4);
}

GMount* Pure_g_file_find_enclosing_mount_finish(GFile* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_find_enclosing_mount_finish(arg0, arg1, arg2);
}

GFileEnumerator* Pure_g_file_enumerate_children(GFile* arg0, char const* arg1, unsigned int arg2, GCancellable* arg3, GError** arg4)
{
  return g_file_enumerate_children(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_file_enumerate_children_async(GFile* arg0, char const* arg1, unsigned int arg2, int arg3, GCancellable* arg4, void* arg5, void* arg6)
{
  return g_file_enumerate_children_async(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

GFileEnumerator* Pure_g_file_enumerate_children_finish(GFile* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_enumerate_children_finish(arg0, arg1, arg2);
}

GFile* Pure_g_file_set_display_name(GFile* arg0, char const* arg1, GCancellable* arg2, GError** arg3)
{
  return g_file_set_display_name(arg0, arg1, arg2, arg3);
}

void Pure_g_file_set_display_name_async(GFile* arg0, char const* arg1, int arg2, GCancellable* arg3, void* arg4, void* arg5)
{
  return g_file_set_display_name_async(arg0, arg1, arg2, arg3, arg4, arg5);
}

GFile* Pure_g_file_set_display_name_finish(GFile* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_set_display_name_finish(arg0, arg1, arg2);
}

int Pure_g_file_delete(GFile* arg0, GCancellable* arg1, GError** arg2)
{
  return g_file_delete(arg0, arg1, arg2);
}

int Pure_g_file_trash(GFile* arg0, GCancellable* arg1, GError** arg2)
{
  return g_file_trash(arg0, arg1, arg2);
}

int Pure_g_file_copy(GFile* arg0, GFile* arg1, unsigned int arg2, GCancellable* arg3, void* arg4, void* arg5, GError** arg6)
{
  return g_file_copy(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

void Pure_g_file_copy_async(GFile* arg0, GFile* arg1, unsigned int arg2, int arg3, GCancellable* arg4, void* arg5, void* arg6, void* arg7, void* arg8)
{
  return g_file_copy_async(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

int Pure_g_file_copy_finish(GFile* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_copy_finish(arg0, arg1, arg2);
}

int Pure_g_file_move(GFile* arg0, GFile* arg1, unsigned int arg2, GCancellable* arg3, void* arg4, void* arg5, GError** arg6)
{
  return g_file_move(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

int Pure_g_file_make_directory(GFile* arg0, GCancellable* arg1, GError** arg2)
{
  return g_file_make_directory(arg0, arg1, arg2);
}

int Pure_g_file_make_directory_with_parents(GFile* arg0, GCancellable* arg1, GError** arg2)
{
  return g_file_make_directory_with_parents(arg0, arg1, arg2);
}

int Pure_g_file_make_symbolic_link(GFile* arg0, char const* arg1, GCancellable* arg2, GError** arg3)
{
  return g_file_make_symbolic_link(arg0, arg1, arg2, arg3);
}

GFileAttributeInfoList* Pure_g_file_query_settable_attributes(GFile* arg0, GCancellable* arg1, GError** arg2)
{
  return g_file_query_settable_attributes(arg0, arg1, arg2);
}

GFileAttributeInfoList* Pure_g_file_query_writable_namespaces(GFile* arg0, GCancellable* arg1, GError** arg2)
{
  return g_file_query_writable_namespaces(arg0, arg1, arg2);
}

int Pure_g_file_set_attribute(GFile* arg0, char const* arg1, unsigned int arg2, void* arg3, unsigned int arg4, GCancellable* arg5, GError** arg6)
{
  return g_file_set_attribute(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

int Pure_g_file_set_attributes_from_info(GFile* arg0, GFileInfo* arg1, unsigned int arg2, GCancellable* arg3, GError** arg4)
{
  return g_file_set_attributes_from_info(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_file_set_attributes_async(GFile* arg0, GFileInfo* arg1, unsigned int arg2, int arg3, GCancellable* arg4, void* arg5, void* arg6)
{
  return g_file_set_attributes_async(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

int Pure_g_file_set_attributes_finish(GFile* arg0, GAsyncResult* arg1, GFileInfo** arg2, GError** arg3)
{
  return g_file_set_attributes_finish(arg0, arg1, arg2, arg3);
}

int Pure_g_file_set_attribute_string(GFile* arg0, char const* arg1, char const* arg2, unsigned int arg3, GCancellable* arg4, GError** arg5)
{
  return g_file_set_attribute_string(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_g_file_set_attribute_byte_string(GFile* arg0, char const* arg1, char const* arg2, unsigned int arg3, GCancellable* arg4, GError** arg5)
{
  return g_file_set_attribute_byte_string(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_g_file_set_attribute_uint32(GFile* arg0, char const* arg1, unsigned int arg2, unsigned int arg3, GCancellable* arg4, GError** arg5)
{
  return g_file_set_attribute_uint32(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_g_file_set_attribute_int32(GFile* arg0, char const* arg1, int arg2, unsigned int arg3, GCancellable* arg4, GError** arg5)
{
  return g_file_set_attribute_int32(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_g_file_set_attribute_uint64(GFile* arg0, char const* arg1, unsigned long arg2, unsigned int arg3, GCancellable* arg4, GError** arg5)
{
  return g_file_set_attribute_uint64(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_g_file_set_attribute_int64(GFile* arg0, char const* arg1, long arg2, unsigned int arg3, GCancellable* arg4, GError** arg5)
{
  return g_file_set_attribute_int64(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_file_mount_enclosing_volume(GFile* arg0, unsigned int arg1, GMountOperation* arg2, GCancellable* arg3, void* arg4, void* arg5)
{
  return g_file_mount_enclosing_volume(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_g_file_mount_enclosing_volume_finish(GFile* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_mount_enclosing_volume_finish(arg0, arg1, arg2);
}

void Pure_g_file_mount_mountable(GFile* arg0, unsigned int arg1, GMountOperation* arg2, GCancellable* arg3, void* arg4, void* arg5)
{
  return g_file_mount_mountable(arg0, arg1, arg2, arg3, arg4, arg5);
}

GFile* Pure_g_file_mount_mountable_finish(GFile* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_mount_mountable_finish(arg0, arg1, arg2);
}

void Pure_g_file_unmount_mountable(GFile* arg0, unsigned int arg1, GCancellable* arg2, void* arg3, void* arg4)
{
  return g_file_unmount_mountable(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_file_unmount_mountable_finish(GFile* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_unmount_mountable_finish(arg0, arg1, arg2);
}

void Pure_g_file_eject_mountable(GFile* arg0, unsigned int arg1, GCancellable* arg2, void* arg3, void* arg4)
{
  return g_file_eject_mountable(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_file_eject_mountable_finish(GFile* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_eject_mountable_finish(arg0, arg1, arg2);
}

int Pure_g_file_copy_attributes(GFile* arg0, GFile* arg1, unsigned int arg2, GCancellable* arg3, GError** arg4)
{
  return g_file_copy_attributes(arg0, arg1, arg2, arg3, arg4);
}

GFileMonitor* Pure_g_file_monitor_directory(GFile* arg0, unsigned int arg1, GCancellable* arg2, GError** arg3)
{
  return g_file_monitor_directory(arg0, arg1, arg2, arg3);
}

GFileMonitor* Pure_g_file_monitor_file(GFile* arg0, unsigned int arg1, GCancellable* arg2, GError** arg3)
{
  return g_file_monitor_file(arg0, arg1, arg2, arg3);
}

GFileMonitor* Pure_g_file_monitor(GFile* arg0, unsigned int arg1, GCancellable* arg2, GError** arg3)
{
  return g_file_monitor(arg0, arg1, arg2, arg3);
}

GAppInfo* Pure_g_file_query_default_handler(GFile* arg0, GCancellable* arg1, GError** arg2)
{
  return g_file_query_default_handler(arg0, arg1, arg2);
}

int Pure_g_file_load_contents(GFile* arg0, GCancellable* arg1, char** arg2, unsigned long* arg3, char** arg4, GError** arg5)
{
  return g_file_load_contents(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_file_load_contents_async(GFile* arg0, GCancellable* arg1, void* arg2, void* arg3)
{
  return g_file_load_contents_async(arg0, arg1, arg2, arg3);
}

int Pure_g_file_load_contents_finish(GFile* arg0, GAsyncResult* arg1, char** arg2, unsigned long* arg3, char** arg4, GError** arg5)
{
  return g_file_load_contents_finish(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_file_load_partial_contents_async(GFile* arg0, GCancellable* arg1, void* arg2, void* arg3, void* arg4)
{
  return g_file_load_partial_contents_async(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_file_load_partial_contents_finish(GFile* arg0, GAsyncResult* arg1, char** arg2, unsigned long* arg3, char** arg4, GError** arg5)
{
  return g_file_load_partial_contents_finish(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_g_file_replace_contents(GFile* arg0, char const* arg1, unsigned long arg2, char const* arg3, int arg4, unsigned int arg5, char** arg6, GCancellable* arg7, GError** arg8)
{
  return g_file_replace_contents(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

void Pure_g_file_replace_contents_async(GFile* arg0, char const* arg1, unsigned long arg2, char const* arg3, int arg4, unsigned int arg5, GCancellable* arg6, void* arg7, void* arg8)
{
  return g_file_replace_contents_async(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

int Pure_g_file_replace_contents_finish(GFile* arg0, GAsyncResult* arg1, char** arg2, GError** arg3)
{
  return g_file_replace_contents_finish(arg0, arg1, arg2, arg3);
}

GFileAttributeInfoList* Pure_g_file_attribute_info_list_new()
{
  return g_file_attribute_info_list_new();
}

GFileAttributeInfoList* Pure_g_file_attribute_info_list_ref(GFileAttributeInfoList* arg0)
{
  return g_file_attribute_info_list_ref(arg0);
}

void Pure_g_file_attribute_info_list_unref(GFileAttributeInfoList* arg0)
{
  return g_file_attribute_info_list_unref(arg0);
}

GFileAttributeInfoList* Pure_g_file_attribute_info_list_dup(GFileAttributeInfoList* arg0)
{
  return g_file_attribute_info_list_dup(arg0);
}

GFileAttributeInfo const* Pure_g_file_attribute_info_list_lookup(GFileAttributeInfoList* arg0, char const* arg1)
{
  return g_file_attribute_info_list_lookup(arg0, arg1);
}

void Pure_g_file_attribute_info_list_add(GFileAttributeInfoList* arg0, char const* arg1, unsigned int arg2, unsigned int arg3)
{
  return g_file_attribute_info_list_add(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_file_enumerator_get_type()
{
  return g_file_enumerator_get_type();
}

GFileInfo* Pure_g_file_enumerator_next_file(GFileEnumerator* arg0, GCancellable* arg1, GError** arg2)
{
  return g_file_enumerator_next_file(arg0, arg1, arg2);
}

int Pure_g_file_enumerator_close(GFileEnumerator* arg0, GCancellable* arg1, GError** arg2)
{
  return g_file_enumerator_close(arg0, arg1, arg2);
}

void Pure_g_file_enumerator_next_files_async(GFileEnumerator* arg0, int arg1, int arg2, GCancellable* arg3, void* arg4, void* arg5)
{
  return g_file_enumerator_next_files_async(arg0, arg1, arg2, arg3, arg4, arg5);
}

GList* Pure_g_file_enumerator_next_files_finish(GFileEnumerator* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_enumerator_next_files_finish(arg0, arg1, arg2);
}

void Pure_g_file_enumerator_close_async(GFileEnumerator* arg0, int arg1, GCancellable* arg2, void* arg3, void* arg4)
{
  return g_file_enumerator_close_async(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_file_enumerator_close_finish(GFileEnumerator* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_enumerator_close_finish(arg0, arg1, arg2);
}

int Pure_g_file_enumerator_is_closed(GFileEnumerator* arg0)
{
  return g_file_enumerator_is_closed(arg0);
}

int Pure_g_file_enumerator_has_pending(GFileEnumerator* arg0)
{
  return g_file_enumerator_has_pending(arg0);
}

void Pure_g_file_enumerator_set_pending(GFileEnumerator* arg0, int arg1)
{
  return g_file_enumerator_set_pending(arg0, arg1);
}

GFile* Pure_g_file_enumerator_get_container(GFileEnumerator* arg0)
{
  return g_file_enumerator_get_container(arg0);
}

unsigned long Pure_g_file_icon_get_type()
{
  return g_file_icon_get_type();
}

GIcon* Pure_g_file_icon_new(GFile* arg0)
{
  return g_file_icon_new(arg0);
}

GFile* Pure_g_file_icon_get_file(GFileIcon* arg0)
{
  return g_file_icon_get_file(arg0);
}

unsigned long Pure_g_file_info_get_type()
{
  return g_file_info_get_type();
}

GFileInfo* Pure_g_file_info_new()
{
  return g_file_info_new();
}

GFileInfo* Pure_g_file_info_dup(GFileInfo* arg0)
{
  return g_file_info_dup(arg0);
}

void Pure_g_file_info_copy_into(GFileInfo* arg0, GFileInfo* arg1)
{
  return g_file_info_copy_into(arg0, arg1);
}

int Pure_g_file_info_has_attribute(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_has_attribute(arg0, arg1);
}

char** Pure_g_file_info_list_attributes(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_list_attributes(arg0, arg1);
}

int Pure_g_file_info_get_attribute_data(GFileInfo* arg0, char const* arg1, unsigned int* arg2, void** arg3, unsigned int* arg4)
{
  return g_file_info_get_attribute_data(arg0, arg1, arg2, arg3, arg4);
}

unsigned int Pure_g_file_info_get_attribute_type(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_get_attribute_type(arg0, arg1);
}

void Pure_g_file_info_remove_attribute(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_remove_attribute(arg0, arg1);
}

unsigned int Pure_g_file_info_get_attribute_status(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_get_attribute_status(arg0, arg1);
}

char* Pure_g_file_info_get_attribute_as_string(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_get_attribute_as_string(arg0, arg1);
}

char const* Pure_g_file_info_get_attribute_string(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_get_attribute_string(arg0, arg1);
}

char const* Pure_g_file_info_get_attribute_byte_string(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_get_attribute_byte_string(arg0, arg1);
}

int Pure_g_file_info_get_attribute_boolean(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_get_attribute_boolean(arg0, arg1);
}

unsigned int Pure_g_file_info_get_attribute_uint32(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_get_attribute_uint32(arg0, arg1);
}

int Pure_g_file_info_get_attribute_int32(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_get_attribute_int32(arg0, arg1);
}

unsigned long Pure_g_file_info_get_attribute_uint64(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_get_attribute_uint64(arg0, arg1);
}

long Pure_g_file_info_get_attribute_int64(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_get_attribute_int64(arg0, arg1);
}

GObject* Pure_g_file_info_get_attribute_object(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_get_attribute_object(arg0, arg1);
}

void Pure_g_file_info_set_attribute(GFileInfo* arg0, char const* arg1, unsigned int arg2, void* arg3)
{
  return g_file_info_set_attribute(arg0, arg1, arg2, arg3);
}

void Pure_g_file_info_set_attribute_string(GFileInfo* arg0, char const* arg1, char const* arg2)
{
  return g_file_info_set_attribute_string(arg0, arg1, arg2);
}

void Pure_g_file_info_set_attribute_byte_string(GFileInfo* arg0, char const* arg1, char const* arg2)
{
  return g_file_info_set_attribute_byte_string(arg0, arg1, arg2);
}

void Pure_g_file_info_set_attribute_boolean(GFileInfo* arg0, char const* arg1, int arg2)
{
  return g_file_info_set_attribute_boolean(arg0, arg1, arg2);
}

void Pure_g_file_info_set_attribute_uint32(GFileInfo* arg0, char const* arg1, unsigned int arg2)
{
  return g_file_info_set_attribute_uint32(arg0, arg1, arg2);
}

void Pure_g_file_info_set_attribute_int32(GFileInfo* arg0, char const* arg1, int arg2)
{
  return g_file_info_set_attribute_int32(arg0, arg1, arg2);
}

void Pure_g_file_info_set_attribute_uint64(GFileInfo* arg0, char const* arg1, unsigned long arg2)
{
  return g_file_info_set_attribute_uint64(arg0, arg1, arg2);
}

void Pure_g_file_info_set_attribute_int64(GFileInfo* arg0, char const* arg1, long arg2)
{
  return g_file_info_set_attribute_int64(arg0, arg1, arg2);
}

void Pure_g_file_info_set_attribute_object(GFileInfo* arg0, char const* arg1, GObject* arg2)
{
  return g_file_info_set_attribute_object(arg0, arg1, arg2);
}

void Pure_g_file_info_clear_status(GFileInfo* arg0)
{
  return g_file_info_clear_status(arg0);
}

unsigned int Pure_g_file_info_get_file_type(GFileInfo* arg0)
{
  return g_file_info_get_file_type(arg0);
}

int Pure_g_file_info_get_is_hidden(GFileInfo* arg0)
{
  return g_file_info_get_is_hidden(arg0);
}

int Pure_g_file_info_get_is_backup(GFileInfo* arg0)
{
  return g_file_info_get_is_backup(arg0);
}

int Pure_g_file_info_get_is_symlink(GFileInfo* arg0)
{
  return g_file_info_get_is_symlink(arg0);
}

char const* Pure_g_file_info_get_name(GFileInfo* arg0)
{
  return g_file_info_get_name(arg0);
}

char const* Pure_g_file_info_get_display_name(GFileInfo* arg0)
{
  return g_file_info_get_display_name(arg0);
}

char const* Pure_g_file_info_get_edit_name(GFileInfo* arg0)
{
  return g_file_info_get_edit_name(arg0);
}

GIcon* Pure_g_file_info_get_icon(GFileInfo* arg0)
{
  return g_file_info_get_icon(arg0);
}

char const* Pure_g_file_info_get_content_type(GFileInfo* arg0)
{
  return g_file_info_get_content_type(arg0);
}

long Pure_g_file_info_get_size(GFileInfo* arg0)
{
  return g_file_info_get_size(arg0);
}

void Pure_g_file_info_get_modification_time(GFileInfo* arg0, GTimeVal* arg1)
{
  return g_file_info_get_modification_time(arg0, arg1);
}

char const* Pure_g_file_info_get_symlink_target(GFileInfo* arg0)
{
  return g_file_info_get_symlink_target(arg0);
}

char const* Pure_g_file_info_get_etag(GFileInfo* arg0)
{
  return g_file_info_get_etag(arg0);
}

int Pure_g_file_info_get_sort_order(GFileInfo* arg0)
{
  return g_file_info_get_sort_order(arg0);
}

void Pure_g_file_info_set_attribute_mask(GFileInfo* arg0, GFileAttributeMatcher* arg1)
{
  return g_file_info_set_attribute_mask(arg0, arg1);
}

void Pure_g_file_info_unset_attribute_mask(GFileInfo* arg0)
{
  return g_file_info_unset_attribute_mask(arg0);
}

void Pure_g_file_info_set_file_type(GFileInfo* arg0, unsigned int arg1)
{
  return g_file_info_set_file_type(arg0, arg1);
}

void Pure_g_file_info_set_is_hidden(GFileInfo* arg0, int arg1)
{
  return g_file_info_set_is_hidden(arg0, arg1);
}

void Pure_g_file_info_set_is_symlink(GFileInfo* arg0, int arg1)
{
  return g_file_info_set_is_symlink(arg0, arg1);
}

void Pure_g_file_info_set_name(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_set_name(arg0, arg1);
}

void Pure_g_file_info_set_display_name(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_set_display_name(arg0, arg1);
}

void Pure_g_file_info_set_edit_name(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_set_edit_name(arg0, arg1);
}

void Pure_g_file_info_set_icon(GFileInfo* arg0, GIcon* arg1)
{
  return g_file_info_set_icon(arg0, arg1);
}

void Pure_g_file_info_set_content_type(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_set_content_type(arg0, arg1);
}

void Pure_g_file_info_set_size(GFileInfo* arg0, long arg1)
{
  return g_file_info_set_size(arg0, arg1);
}

void Pure_g_file_info_set_modification_time(GFileInfo* arg0, GTimeVal* arg1)
{
  return g_file_info_set_modification_time(arg0, arg1);
}

void Pure_g_file_info_set_symlink_target(GFileInfo* arg0, char const* arg1)
{
  return g_file_info_set_symlink_target(arg0, arg1);
}

void Pure_g_file_info_set_sort_order(GFileInfo* arg0, int arg1)
{
  return g_file_info_set_sort_order(arg0, arg1);
}

GFileAttributeMatcher* Pure_g_file_attribute_matcher_new(char const* arg0)
{
  return g_file_attribute_matcher_new(arg0);
}

GFileAttributeMatcher* Pure_g_file_attribute_matcher_ref(GFileAttributeMatcher* arg0)
{
  return g_file_attribute_matcher_ref(arg0);
}

void Pure_g_file_attribute_matcher_unref(GFileAttributeMatcher* arg0)
{
  return g_file_attribute_matcher_unref(arg0);
}

int Pure_g_file_attribute_matcher_matches(GFileAttributeMatcher* arg0, char const* arg1)
{
  return g_file_attribute_matcher_matches(arg0, arg1);
}

int Pure_g_file_attribute_matcher_matches_only(GFileAttributeMatcher* arg0, char const* arg1)
{
  return g_file_attribute_matcher_matches_only(arg0, arg1);
}

int Pure_g_file_attribute_matcher_enumerate_namespace(GFileAttributeMatcher* arg0, char const* arg1)
{
  return g_file_attribute_matcher_enumerate_namespace(arg0, arg1);
}

char const* Pure_g_file_attribute_matcher_enumerate_next(GFileAttributeMatcher* arg0)
{
  return g_file_attribute_matcher_enumerate_next(arg0);
}

unsigned long Pure_g_file_input_stream_get_type()
{
  return g_file_input_stream_get_type();
}

GFileInfo* Pure_g_file_input_stream_query_info(GFileInputStream* arg0, char* arg1, GCancellable* arg2, GError** arg3)
{
  return g_file_input_stream_query_info(arg0, arg1, arg2, arg3);
}

void Pure_g_file_input_stream_query_info_async(GFileInputStream* arg0, char* arg1, int arg2, GCancellable* arg3, void* arg4, void* arg5)
{
  return g_file_input_stream_query_info_async(arg0, arg1, arg2, arg3, arg4, arg5);
}

GFileInfo* Pure_g_file_input_stream_query_info_finish(GFileInputStream* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_input_stream_query_info_finish(arg0, arg1, arg2);
}

unsigned long Pure_g_file_monitor_get_type()
{
  return g_file_monitor_get_type();
}

int Pure_g_file_monitor_cancel(GFileMonitor* arg0)
{
  return g_file_monitor_cancel(arg0);
}

int Pure_g_file_monitor_is_cancelled(GFileMonitor* arg0)
{
  return g_file_monitor_is_cancelled(arg0);
}

void Pure_g_file_monitor_set_rate_limit(GFileMonitor* arg0, int arg1)
{
  return g_file_monitor_set_rate_limit(arg0, arg1);
}

void Pure_g_file_monitor_emit_event(GFileMonitor* arg0, GFile* arg1, GFile* arg2, unsigned int arg3)
{
  return g_file_monitor_emit_event(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_filename_completer_get_type()
{
  return g_filename_completer_get_type();
}

GFilenameCompleter* Pure_g_filename_completer_new()
{
  return g_filename_completer_new();
}

char* Pure_g_filename_completer_get_completion_suffix(GFilenameCompleter* arg0, char const* arg1)
{
  return g_filename_completer_get_completion_suffix(arg0, arg1);
}

char** Pure_g_filename_completer_get_completions(GFilenameCompleter* arg0, char const* arg1)
{
  return g_filename_completer_get_completions(arg0, arg1);
}

void Pure_g_filename_completer_set_dirs_only(GFilenameCompleter* arg0, int arg1)
{
  return g_filename_completer_set_dirs_only(arg0, arg1);
}

unsigned long Pure_g_file_output_stream_get_type()
{
  return g_file_output_stream_get_type();
}

GFileInfo* Pure_g_file_output_stream_query_info(GFileOutputStream* arg0, char* arg1, GCancellable* arg2, GError** arg3)
{
  return g_file_output_stream_query_info(arg0, arg1, arg2, arg3);
}

void Pure_g_file_output_stream_query_info_async(GFileOutputStream* arg0, char* arg1, int arg2, GCancellable* arg3, void* arg4, void* arg5)
{
  return g_file_output_stream_query_info_async(arg0, arg1, arg2, arg3, arg4, arg5);
}

GFileInfo* Pure_g_file_output_stream_query_info_finish(GFileOutputStream* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_file_output_stream_query_info_finish(arg0, arg1, arg2);
}

char* Pure_g_file_output_stream_get_etag(GFileOutputStream* arg0)
{
  return g_file_output_stream_get_etag(arg0);
}

unsigned long Pure_g_app_info_create_flags_get_type()
{
  return g_app_info_create_flags_get_type();
}

unsigned long Pure_g_data_stream_byte_order_get_type()
{
  return g_data_stream_byte_order_get_type();
}

unsigned long Pure_g_data_stream_newline_type_get_type()
{
  return g_data_stream_newline_type_get_type();
}

unsigned long Pure_g_file_attribute_type_get_type()
{
  return g_file_attribute_type_get_type();
}

unsigned long Pure_g_file_attribute_info_flags_get_type()
{
  return g_file_attribute_info_flags_get_type();
}

unsigned long Pure_g_file_attribute_status_get_type()
{
  return g_file_attribute_status_get_type();
}

unsigned long Pure_g_file_query_info_flags_get_type()
{
  return g_file_query_info_flags_get_type();
}

unsigned long Pure_g_file_create_flags_get_type()
{
  return g_file_create_flags_get_type();
}

unsigned long Pure_g_mount_mount_flags_get_type()
{
  return g_mount_mount_flags_get_type();
}

unsigned long Pure_g_mount_unmount_flags_get_type()
{
  return g_mount_unmount_flags_get_type();
}

unsigned long Pure_g_file_copy_flags_get_type()
{
  return g_file_copy_flags_get_type();
}

unsigned long Pure_g_file_monitor_flags_get_type()
{
  return g_file_monitor_flags_get_type();
}

unsigned long Pure_g_file_type_get_type()
{
  return g_file_type_get_type();
}

unsigned long Pure_g_filesystem_preview_type_get_type()
{
  return g_filesystem_preview_type_get_type();
}

unsigned long Pure_g_file_monitor_event_get_type()
{
  return g_file_monitor_event_get_type();
}

unsigned long Pure_g_io_error_enum_get_type()
{
  return g_io_error_enum_get_type();
}

unsigned long Pure_g_ask_password_flags_get_type()
{
  return g_ask_password_flags_get_type();
}

unsigned long Pure_g_password_save_get_type()
{
  return g_password_save_get_type();
}

unsigned long Pure_g_mount_operation_result_get_type()
{
  return g_mount_operation_result_get_type();
}

unsigned long Pure_g_output_stream_splice_flags_get_type()
{
  return g_output_stream_splice_flags_get_type();
}

unsigned long Pure_g_emblem_origin_get_type()
{
  return g_emblem_origin_get_type();
}

unsigned int Pure_g_io_error_quark()
{
  return g_io_error_quark();
}

unsigned int Pure_g_io_error_from_errno(int arg0)
{
  return g_io_error_from_errno(arg0);
}

unsigned long Pure_g_io_module_get_type()
{
  return g_io_module_get_type();
}

GIOModule* Pure_g_io_module_new(char const* arg0)
{
  return g_io_module_new(arg0);
}

GList* Pure_g_io_modules_load_all_in_directory(char const* arg0)
{
  return g_io_modules_load_all_in_directory(arg0);
}

GIOExtensionPoint* Pure_g_io_extension_point_register(char const* arg0)
{
  return g_io_extension_point_register(arg0);
}

GIOExtensionPoint* Pure_g_io_extension_point_lookup(char const* arg0)
{
  return g_io_extension_point_lookup(arg0);
}

void Pure_g_io_extension_point_set_required_type(GIOExtensionPoint* arg0, unsigned long arg1)
{
  return g_io_extension_point_set_required_type(arg0, arg1);
}

unsigned long Pure_g_io_extension_point_get_required_type(GIOExtensionPoint* arg0)
{
  return g_io_extension_point_get_required_type(arg0);
}

GList* Pure_g_io_extension_point_get_extensions(GIOExtensionPoint* arg0)
{
  return g_io_extension_point_get_extensions(arg0);
}

GIOExtension* Pure_g_io_extension_point_get_extension_by_name(GIOExtensionPoint* arg0, char const* arg1)
{
  return g_io_extension_point_get_extension_by_name(arg0, arg1);
}

GIOExtension* Pure_g_io_extension_point_implement(char const* arg0, unsigned long arg1, char const* arg2, int arg3)
{
  return g_io_extension_point_implement(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_io_extension_get_type(GIOExtension* arg0)
{
  return g_io_extension_get_type(arg0);
}

char const* Pure_g_io_extension_get_name(GIOExtension* arg0)
{
  return g_io_extension_get_name(arg0);
}

int Pure_g_io_extension_get_priority(GIOExtension* arg0)
{
  return g_io_extension_get_priority(arg0);
}

GTypeClass* Pure_g_io_extension_ref_class(GIOExtension* arg0)
{
  return g_io_extension_ref_class(arg0);
}

void Pure_g_io_scheduler_push_job(void* arg0, void* arg1, void* arg2, int arg3, GCancellable* arg4)
{
  return g_io_scheduler_push_job(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_io_scheduler_cancel_all_jobs()
{
  return g_io_scheduler_cancel_all_jobs();
}

int Pure_g_io_scheduler_job_send_to_mainloop(GIOSchedulerJob* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_io_scheduler_job_send_to_mainloop(arg0, arg1, arg2, arg3);
}

void Pure_g_io_scheduler_job_send_to_mainloop_async(GIOSchedulerJob* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_io_scheduler_job_send_to_mainloop_async(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_loadable_icon_get_type()
{
  return g_loadable_icon_get_type();
}

GInputStream* Pure_g_loadable_icon_load(GLoadableIcon* arg0, int arg1, char** arg2, GCancellable* arg3, GError** arg4)
{
  return g_loadable_icon_load(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_loadable_icon_load_async(GLoadableIcon* arg0, int arg1, GCancellable* arg2, void* arg3, void* arg4)
{
  return g_loadable_icon_load_async(arg0, arg1, arg2, arg3, arg4);
}

GInputStream* Pure_g_loadable_icon_load_finish(GLoadableIcon* arg0, GAsyncResult* arg1, char** arg2, GError** arg3)
{
  return g_loadable_icon_load_finish(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_memory_input_stream_get_type()
{
  return g_memory_input_stream_get_type();
}

GInputStream* Pure_g_memory_input_stream_new()
{
  return g_memory_input_stream_new();
}

GInputStream* Pure_g_memory_input_stream_new_from_data(void const* arg0, long arg1, void* arg2)
{
  return g_memory_input_stream_new_from_data(arg0, arg1, arg2);
}

void Pure_g_memory_input_stream_add_data(GMemoryInputStream* arg0, void const* arg1, long arg2, void* arg3)
{
  return g_memory_input_stream_add_data(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_memory_output_stream_get_type()
{
  return g_memory_output_stream_get_type();
}

GOutputStream* Pure_g_memory_output_stream_new(void* arg0, unsigned long arg1, void* arg2, void* arg3)
{
  return g_memory_output_stream_new(arg0, arg1, arg2, arg3);
}

void* Pure_g_memory_output_stream_get_data(GMemoryOutputStream* arg0)
{
  return g_memory_output_stream_get_data(arg0);
}

unsigned long Pure_g_memory_output_stream_get_size(GMemoryOutputStream* arg0)
{
  return g_memory_output_stream_get_size(arg0);
}

unsigned long Pure_g_memory_output_stream_get_data_size(GMemoryOutputStream* arg0)
{
  return g_memory_output_stream_get_data_size(arg0);
}

unsigned long Pure_g_mount_get_type()
{
  return g_mount_get_type();
}

GFile* Pure_g_mount_get_root(GMount* arg0)
{
  return g_mount_get_root(arg0);
}

char* Pure_g_mount_get_name(GMount* arg0)
{
  return g_mount_get_name(arg0);
}

GIcon* Pure_g_mount_get_icon(GMount* arg0)
{
  return g_mount_get_icon(arg0);
}

char* Pure_g_mount_get_uuid(GMount* arg0)
{
  return g_mount_get_uuid(arg0);
}

GVolume* Pure_g_mount_get_volume(GMount* arg0)
{
  return g_mount_get_volume(arg0);
}

GDrive* Pure_g_mount_get_drive(GMount* arg0)
{
  return g_mount_get_drive(arg0);
}

int Pure_g_mount_can_unmount(GMount* arg0)
{
  return g_mount_can_unmount(arg0);
}

int Pure_g_mount_can_eject(GMount* arg0)
{
  return g_mount_can_eject(arg0);
}

void Pure_g_mount_unmount(GMount* arg0, unsigned int arg1, GCancellable* arg2, void* arg3, void* arg4)
{
  return g_mount_unmount(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_mount_unmount_finish(GMount* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_mount_unmount_finish(arg0, arg1, arg2);
}

void Pure_g_mount_eject(GMount* arg0, unsigned int arg1, GCancellable* arg2, void* arg3, void* arg4)
{
  return g_mount_eject(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_mount_eject_finish(GMount* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_mount_eject_finish(arg0, arg1, arg2);
}

void Pure_g_mount_remount(GMount* arg0, unsigned int arg1, GMountOperation* arg2, GCancellable* arg3, void* arg4, void* arg5)
{
  return g_mount_remount(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_g_mount_remount_finish(GMount* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_mount_remount_finish(arg0, arg1, arg2);
}

void Pure_g_mount_guess_content_type(GMount* arg0, int arg1, GCancellable* arg2, void* arg3, void* arg4)
{
  return g_mount_guess_content_type(arg0, arg1, arg2, arg3, arg4);
}

char** Pure_g_mount_guess_content_type_finish(GMount* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_mount_guess_content_type_finish(arg0, arg1, arg2);
}

char** Pure_g_mount_guess_content_type_sync(GMount* arg0, int arg1, GCancellable* arg2, GError** arg3)
{
  return g_mount_guess_content_type_sync(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_mount_operation_get_type()
{
  return g_mount_operation_get_type();
}

GMountOperation* Pure_g_mount_operation_new()
{
  return g_mount_operation_new();
}

char const* Pure_g_mount_operation_get_username(GMountOperation* arg0)
{
  return g_mount_operation_get_username(arg0);
}

void Pure_g_mount_operation_set_username(GMountOperation* arg0, char const* arg1)
{
  return g_mount_operation_set_username(arg0, arg1);
}

char const* Pure_g_mount_operation_get_password(GMountOperation* arg0)
{
  return g_mount_operation_get_password(arg0);
}

void Pure_g_mount_operation_set_password(GMountOperation* arg0, char const* arg1)
{
  return g_mount_operation_set_password(arg0, arg1);
}

int Pure_g_mount_operation_get_anonymous(GMountOperation* arg0)
{
  return g_mount_operation_get_anonymous(arg0);
}

void Pure_g_mount_operation_set_anonymous(GMountOperation* arg0, int arg1)
{
  return g_mount_operation_set_anonymous(arg0, arg1);
}

char const* Pure_g_mount_operation_get_domain(GMountOperation* arg0)
{
  return g_mount_operation_get_domain(arg0);
}

void Pure_g_mount_operation_set_domain(GMountOperation* arg0, char const* arg1)
{
  return g_mount_operation_set_domain(arg0, arg1);
}

unsigned int Pure_g_mount_operation_get_password_save(GMountOperation* arg0)
{
  return g_mount_operation_get_password_save(arg0);
}

void Pure_g_mount_operation_set_password_save(GMountOperation* arg0, unsigned int arg1)
{
  return g_mount_operation_set_password_save(arg0, arg1);
}

int Pure_g_mount_operation_get_choice(GMountOperation* arg0)
{
  return g_mount_operation_get_choice(arg0);
}

void Pure_g_mount_operation_set_choice(GMountOperation* arg0, int arg1)
{
  return g_mount_operation_set_choice(arg0, arg1);
}

void Pure_g_mount_operation_reply(GMountOperation* arg0, unsigned int arg1)
{
  return g_mount_operation_reply(arg0, arg1);
}

unsigned long Pure_g_volume_monitor_get_type()
{
  return g_volume_monitor_get_type();
}

GVolumeMonitor* Pure_g_volume_monitor_get()
{
  return g_volume_monitor_get();
}

GList* Pure_g_volume_monitor_get_connected_drives(GVolumeMonitor* arg0)
{
  return g_volume_monitor_get_connected_drives(arg0);
}

GList* Pure_g_volume_monitor_get_volumes(GVolumeMonitor* arg0)
{
  return g_volume_monitor_get_volumes(arg0);
}

GList* Pure_g_volume_monitor_get_mounts(GVolumeMonitor* arg0)
{
  return g_volume_monitor_get_mounts(arg0);
}

GVolume* Pure_g_volume_monitor_get_volume_for_uuid(GVolumeMonitor* arg0, char const* arg1)
{
  return g_volume_monitor_get_volume_for_uuid(arg0, arg1);
}

GMount* Pure_g_volume_monitor_get_mount_for_uuid(GVolumeMonitor* arg0, char const* arg1)
{
  return g_volume_monitor_get_mount_for_uuid(arg0, arg1);
}

GVolume* Pure_g_volume_monitor_adopt_orphan_mount(GMount* arg0)
{
  return g_volume_monitor_adopt_orphan_mount(arg0);
}

unsigned long Pure_g_native_volume_monitor_get_type()
{
  return g_native_volume_monitor_get_type();
}

unsigned long Pure_g_seekable_get_type()
{
  return g_seekable_get_type();
}

long Pure_g_seekable_tell(GSeekable* arg0)
{
  return g_seekable_tell(arg0);
}

int Pure_g_seekable_can_seek(GSeekable* arg0)
{
  return g_seekable_can_seek(arg0);
}

int Pure_g_seekable_seek(GSeekable* arg0, long arg1, unsigned int arg2, GCancellable* arg3, GError** arg4)
{
  return g_seekable_seek(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_seekable_can_truncate(GSeekable* arg0)
{
  return g_seekable_can_truncate(arg0);
}

int Pure_g_seekable_truncate(GSeekable* arg0, long arg1, GCancellable* arg2, GError** arg3)
{
  return g_seekable_truncate(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_simple_async_result_get_type()
{
  return g_simple_async_result_get_type();
}

GSimpleAsyncResult* Pure_g_simple_async_result_new(GObject* arg0, void* arg1, void* arg2, void* arg3)
{
  return g_simple_async_result_new(arg0, arg1, arg2, arg3);
}

GSimpleAsyncResult* Pure_g_simple_async_result_new_error(GObject* arg0, void* arg1, void* arg2, unsigned int arg3, int arg4, char const* arg5)
{
  return g_simple_async_result_new_error(arg0, arg1, arg2, arg3, arg4, arg5);
}

GSimpleAsyncResult* Pure_g_simple_async_result_new_from_error(GObject* arg0, void* arg1, void* arg2, GError* arg3)
{
  return g_simple_async_result_new_from_error(arg0, arg1, arg2, arg3);
}

void Pure_g_simple_async_result_set_op_res_gpointer(GSimpleAsyncResult* arg0, void* arg1, void* arg2)
{
  return g_simple_async_result_set_op_res_gpointer(arg0, arg1, arg2);
}

void* Pure_g_simple_async_result_get_op_res_gpointer(GSimpleAsyncResult* arg0)
{
  return g_simple_async_result_get_op_res_gpointer(arg0);
}

void Pure_g_simple_async_result_set_op_res_gssize(GSimpleAsyncResult* arg0, long arg1)
{
  return g_simple_async_result_set_op_res_gssize(arg0, arg1);
}

long Pure_g_simple_async_result_get_op_res_gssize(GSimpleAsyncResult* arg0)
{
  return g_simple_async_result_get_op_res_gssize(arg0);
}

void Pure_g_simple_async_result_set_op_res_gboolean(GSimpleAsyncResult* arg0, int arg1)
{
  return g_simple_async_result_set_op_res_gboolean(arg0, arg1);
}

int Pure_g_simple_async_result_get_op_res_gboolean(GSimpleAsyncResult* arg0)
{
  return g_simple_async_result_get_op_res_gboolean(arg0);
}

void* Pure_g_simple_async_result_get_source_tag(GSimpleAsyncResult* arg0)
{
  return g_simple_async_result_get_source_tag(arg0);
}

void Pure_g_simple_async_result_set_handle_cancellation(GSimpleAsyncResult* arg0, int arg1)
{
  return g_simple_async_result_set_handle_cancellation(arg0, arg1);
}

void Pure_g_simple_async_result_complete(GSimpleAsyncResult* arg0)
{
  return g_simple_async_result_complete(arg0);
}

void Pure_g_simple_async_result_complete_in_idle(GSimpleAsyncResult* arg0)
{
  return g_simple_async_result_complete_in_idle(arg0);
}

void Pure_g_simple_async_result_run_in_thread(GSimpleAsyncResult* arg0, void* arg1, int arg2, GCancellable* arg3)
{
  return g_simple_async_result_run_in_thread(arg0, arg1, arg2, arg3);
}

void Pure_g_simple_async_result_set_from_error(GSimpleAsyncResult* arg0, GError* arg1)
{
  return g_simple_async_result_set_from_error(arg0, arg1);
}

int Pure_g_simple_async_result_propagate_error(GSimpleAsyncResult* arg0, GError** arg1)
{
  return g_simple_async_result_propagate_error(arg0, arg1);
}

void Pure_g_simple_async_result_set_error(GSimpleAsyncResult* arg0, unsigned int arg1, int arg2, char const* arg3)
{
  return g_simple_async_result_set_error(arg0, arg1, arg2, arg3);
}

void Pure_g_simple_async_result_set_error_va(GSimpleAsyncResult* arg0, unsigned int arg1, int arg2, char const* arg3, void* arg4)
{
  return g_simple_async_result_set_error_va(arg0, arg1, arg2, arg3, arg4);
}

void Pure_g_simple_async_report_error_in_idle(GObject* arg0, void* arg1, void* arg2, unsigned int arg3, int arg4, char const* arg5)
{
  return g_simple_async_report_error_in_idle(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_g_simple_async_report_gerror_in_idle(GObject* arg0, void* arg1, void* arg2, GError* arg3)
{
  return g_simple_async_report_gerror_in_idle(arg0, arg1, arg2, arg3);
}

unsigned long Pure_g_themed_icon_get_type()
{
  return g_themed_icon_get_type();
}

GIcon* Pure_g_themed_icon_new(char const* arg0)
{
  return g_themed_icon_new(arg0);
}

GIcon* Pure_g_themed_icon_new_with_default_fallbacks(char const* arg0)
{
  return g_themed_icon_new_with_default_fallbacks(arg0);
}

GIcon* Pure_g_themed_icon_new_from_names(char** arg0, int arg1)
{
  return g_themed_icon_new_from_names(arg0, arg1);
}

void Pure_g_themed_icon_prepend_name(GThemedIcon* arg0, char const* arg1)
{
  return g_themed_icon_prepend_name(arg0, arg1);
}

void Pure_g_themed_icon_append_name(GThemedIcon* arg0, char const* arg1)
{
  return g_themed_icon_append_name(arg0, arg1);
}

char const* const* Pure_g_themed_icon_get_names(GThemedIcon* arg0)
{
  return g_themed_icon_get_names(arg0);
}

unsigned long Pure_g_vfs_get_type()
{
  return g_vfs_get_type();
}

int Pure_g_vfs_is_active(GVfs* arg0)
{
  return g_vfs_is_active(arg0);
}

GFile* Pure_g_vfs_get_file_for_path(GVfs* arg0, char const* arg1)
{
  return g_vfs_get_file_for_path(arg0, arg1);
}

GFile* Pure_g_vfs_get_file_for_uri(GVfs* arg0, char const* arg1)
{
  return g_vfs_get_file_for_uri(arg0, arg1);
}

char const* const* Pure_g_vfs_get_supported_uri_schemes(GVfs* arg0)
{
  return g_vfs_get_supported_uri_schemes(arg0);
}

GFile* Pure_g_vfs_parse_name(GVfs* arg0, char const* arg1)
{
  return g_vfs_parse_name(arg0, arg1);
}

GVfs* Pure_g_vfs_get_default()
{
  return g_vfs_get_default();
}

GVfs* Pure_g_vfs_get_local()
{
  return g_vfs_get_local();
}

unsigned long Pure_g_volume_get_type()
{
  return g_volume_get_type();
}

char* Pure_g_volume_get_name(GVolume* arg0)
{
  return g_volume_get_name(arg0);
}

GIcon* Pure_g_volume_get_icon(GVolume* arg0)
{
  return g_volume_get_icon(arg0);
}

char* Pure_g_volume_get_uuid(GVolume* arg0)
{
  return g_volume_get_uuid(arg0);
}

GDrive* Pure_g_volume_get_drive(GVolume* arg0)
{
  return g_volume_get_drive(arg0);
}

GMount* Pure_g_volume_get_mount(GVolume* arg0)
{
  return g_volume_get_mount(arg0);
}

int Pure_g_volume_can_mount(GVolume* arg0)
{
  return g_volume_can_mount(arg0);
}

int Pure_g_volume_can_eject(GVolume* arg0)
{
  return g_volume_can_eject(arg0);
}

int Pure_g_volume_should_automount(GVolume* arg0)
{
  return g_volume_should_automount(arg0);
}

void Pure_g_volume_mount(GVolume* arg0, unsigned int arg1, GMountOperation* arg2, GCancellable* arg3, void* arg4, void* arg5)
{
  return g_volume_mount(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_g_volume_mount_finish(GVolume* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_volume_mount_finish(arg0, arg1, arg2);
}

void Pure_g_volume_eject(GVolume* arg0, unsigned int arg1, GCancellable* arg2, void* arg3, void* arg4)
{
  return g_volume_eject(arg0, arg1, arg2, arg3, arg4);
}

int Pure_g_volume_eject_finish(GVolume* arg0, GAsyncResult* arg1, GError** arg2)
{
  return g_volume_eject_finish(arg0, arg1, arg2);
}

char* Pure_g_volume_get_identifier(GVolume* arg0, char const* arg1)
{
  return g_volume_get_identifier(arg0, arg1);
}

char** Pure_g_volume_enumerate_identifiers(GVolume* arg0)
{
  return g_volume_enumerate_identifiers(arg0);
}

GFile* Pure_g_volume_get_activation_root(GVolume* arg0)
{
  return g_volume_get_activation_root(arg0);
}
#include <gmodule.h>

int Pure_g_module_supported()
{
  return g_module_supported();
}

GModule* Pure_g_module_open(char const* arg0, unsigned int arg1)
{
  return g_module_open(arg0, arg1);
}

int Pure_g_module_close(GModule* arg0)
{
  return g_module_close(arg0);
}

void Pure_g_module_make_resident(GModule* arg0)
{
  return g_module_make_resident(arg0);
}

char const* Pure_g_module_error()
{
  return g_module_error();
}

int Pure_g_module_symbol(GModule* arg0, char const* arg1, void** arg2)
{
  return g_module_symbol(arg0, arg1, arg2);
}

char const* Pure_g_module_name(GModule* arg0)
{
  return g_module_name(arg0);
}

char* Pure_g_module_build_path(char const* arg0, char const* arg1)
{
  return g_module_build_path(arg0, arg1);
}
