#include "/usr/include/atk-1.0/atk/atk.h"

unsigned int Pure_atk_state_type_register(char const* arg0)
{
  return atk_state_type_register(arg0);
}

char const* Pure_atk_state_type_get_name(unsigned int arg0)
{
  return atk_state_type_get_name(arg0);
}

unsigned int Pure_atk_state_type_for_name(char const* arg0)
{
  return atk_state_type_for_name(arg0);
}

unsigned int Pure_atk_role_register(char const* arg0)
{
  return atk_role_register(arg0);
}

unsigned long Pure_atk_object_get_type()
{
  return atk_object_get_type();
}

unsigned long Pure_atk_implementor_get_type()
{
  return atk_implementor_get_type();
}

AtkObject* Pure_atk_implementor_ref_accessible(AtkImplementor* arg0)
{
  return atk_implementor_ref_accessible(arg0);
}

char const* Pure_atk_object_get_name(AtkObject* arg0)
{
  return atk_object_get_name(arg0);
}

char const* Pure_atk_object_get_description(AtkObject* arg0)
{
  return atk_object_get_description(arg0);
}

AtkObject* Pure_atk_object_get_parent(AtkObject* arg0)
{
  return atk_object_get_parent(arg0);
}

int Pure_atk_object_get_n_accessible_children(AtkObject* arg0)
{
  return atk_object_get_n_accessible_children(arg0);
}

AtkObject* Pure_atk_object_ref_accessible_child(AtkObject* arg0, int arg1)
{
  return atk_object_ref_accessible_child(arg0, arg1);
}

AtkRelationSet* Pure_atk_object_ref_relation_set(AtkObject* arg0)
{
  return atk_object_ref_relation_set(arg0);
}

unsigned int Pure_atk_object_get_role(AtkObject* arg0)
{
  return atk_object_get_role(arg0);
}

unsigned int Pure_atk_object_get_layer(AtkObject* arg0)
{
  return atk_object_get_layer(arg0);
}

int Pure_atk_object_get_mdi_zorder(AtkObject* arg0)
{
  return atk_object_get_mdi_zorder(arg0);
}

AtkAttributeSet* Pure_atk_object_get_attributes(AtkObject* arg0)
{
  return atk_object_get_attributes(arg0);
}

AtkStateSet* Pure_atk_object_ref_state_set(AtkObject* arg0)
{
  return atk_object_ref_state_set(arg0);
}

int Pure_atk_object_get_index_in_parent(AtkObject* arg0)
{
  return atk_object_get_index_in_parent(arg0);
}

void Pure_atk_object_set_name(AtkObject* arg0, char const* arg1)
{
  return atk_object_set_name(arg0, arg1);
}

void Pure_atk_object_set_description(AtkObject* arg0, char const* arg1)
{
  return atk_object_set_description(arg0, arg1);
}

void Pure_atk_object_set_parent(AtkObject* arg0, AtkObject* arg1)
{
  return atk_object_set_parent(arg0, arg1);
}

void Pure_atk_object_set_role(AtkObject* arg0, unsigned int arg1)
{
  return atk_object_set_role(arg0, arg1);
}

unsigned int Pure_atk_object_connect_property_change_handler(AtkObject* arg0, void** arg1)
{
  return atk_object_connect_property_change_handler(arg0, arg1);
}

void Pure_atk_object_remove_property_change_handler(AtkObject* arg0, unsigned int arg1)
{
  return atk_object_remove_property_change_handler(arg0, arg1);
}

void Pure_atk_object_notify_state_change(AtkObject* arg0, unsigned long arg1, int arg2)
{
  return atk_object_notify_state_change(arg0, arg1, arg2);
}

void Pure_atk_object_initialize(AtkObject* arg0, void* arg1)
{
  return atk_object_initialize(arg0, arg1);
}

char const* Pure_atk_role_get_name(unsigned int arg0)
{
  return atk_role_get_name(arg0);
}

unsigned int Pure_atk_role_for_name(char const* arg0)
{
  return atk_role_for_name(arg0);
}

int Pure_atk_object_add_relationship(AtkObject* arg0, unsigned int arg1, AtkObject* arg2)
{
  return atk_object_add_relationship(arg0, arg1, arg2);
}

int Pure_atk_object_remove_relationship(AtkObject* arg0, unsigned int arg1, AtkObject* arg2)
{
  return atk_object_remove_relationship(arg0, arg1, arg2);
}

char const* Pure_atk_role_get_localized_name(unsigned int arg0)
{
  return atk_role_get_localized_name(arg0);
}

unsigned long Pure_atk_action_get_type()
{
  return atk_action_get_type();
}

int Pure_atk_action_do_action(AtkAction* arg0, int arg1)
{
  return atk_action_do_action(arg0, arg1);
}

int Pure_atk_action_get_n_actions(AtkAction* arg0)
{
  return atk_action_get_n_actions(arg0);
}

char const* Pure_atk_action_get_description(AtkAction* arg0, int arg1)
{
  return atk_action_get_description(arg0, arg1);
}

char const* Pure_atk_action_get_name(AtkAction* arg0, int arg1)
{
  return atk_action_get_name(arg0, arg1);
}

char const* Pure_atk_action_get_keybinding(AtkAction* arg0, int arg1)
{
  return atk_action_get_keybinding(arg0, arg1);
}

int Pure_atk_action_set_description(AtkAction* arg0, int arg1, char const* arg2)
{
  return atk_action_set_description(arg0, arg1, arg2);
}

char const* Pure_atk_action_get_localized_name(AtkAction* arg0, int arg1)
{
  return atk_action_get_localized_name(arg0, arg1);
}

unsigned long Pure_atk_util_get_type()
{
  return atk_util_get_type();
}

unsigned int Pure_atk_add_focus_tracker(void* arg0)
{
  return atk_add_focus_tracker(arg0);
}

void Pure_atk_remove_focus_tracker(unsigned int arg0)
{
  return atk_remove_focus_tracker(arg0);
}

void Pure_atk_focus_tracker_init(void* arg0)
{
  return atk_focus_tracker_init(arg0);
}

void Pure_atk_focus_tracker_notify(AtkObject* arg0)
{
  return atk_focus_tracker_notify(arg0);
}

unsigned int Pure_atk_add_global_event_listener(void* arg0, char const* arg1)
{
  return atk_add_global_event_listener(arg0, arg1);
}

void Pure_atk_remove_global_event_listener(unsigned int arg0)
{
  return atk_remove_global_event_listener(arg0);
}

unsigned int Pure_atk_add_key_event_listener(void* arg0, void* arg1)
{
  return atk_add_key_event_listener(arg0, arg1);
}

void Pure_atk_remove_key_event_listener(unsigned int arg0)
{
  return atk_remove_key_event_listener(arg0);
}

AtkObject* Pure_atk_get_root()
{
  return atk_get_root();
}

AtkObject* Pure_atk_get_focus_object()
{
  return atk_get_focus_object();
}

char const* Pure_atk_get_toolkit_name()
{
  return atk_get_toolkit_name();
}

char const* Pure_atk_get_toolkit_version()
{
  return atk_get_toolkit_version();
}

char const* Pure_atk_get_version()
{
  return atk_get_version();
}

unsigned long Pure_atk_rectangle_get_type()
{
  return atk_rectangle_get_type();
}

unsigned long Pure_atk_component_get_type()
{
  return atk_component_get_type();
}

unsigned int Pure_atk_component_add_focus_handler(AtkComponent* arg0, void* arg1)
{
  return atk_component_add_focus_handler(arg0, arg1);
}

int Pure_atk_component_contains(AtkComponent* arg0, int arg1, int arg2, unsigned int arg3)
{
  return atk_component_contains(arg0, arg1, arg2, arg3);
}

AtkObject* Pure_atk_component_ref_accessible_at_point(AtkComponent* arg0, int arg1, int arg2, unsigned int arg3)
{
  return atk_component_ref_accessible_at_point(arg0, arg1, arg2, arg3);
}

void Pure_atk_component_get_extents(AtkComponent* arg0, int* arg1, int* arg2, int* arg3, int* arg4, unsigned int arg5)
{
  return atk_component_get_extents(arg0, arg1, arg2, arg3, arg4, arg5);
}

void Pure_atk_component_get_position(AtkComponent* arg0, int* arg1, int* arg2, unsigned int arg3)
{
  return atk_component_get_position(arg0, arg1, arg2, arg3);
}

void Pure_atk_component_get_size(AtkComponent* arg0, int* arg1, int* arg2)
{
  return atk_component_get_size(arg0, arg1, arg2);
}

unsigned int Pure_atk_component_get_layer(AtkComponent* arg0)
{
  return atk_component_get_layer(arg0);
}

int Pure_atk_component_get_mdi_zorder(AtkComponent* arg0)
{
  return atk_component_get_mdi_zorder(arg0);
}

int Pure_atk_component_grab_focus(AtkComponent* arg0)
{
  return atk_component_grab_focus(arg0);
}

void Pure_atk_component_remove_focus_handler(AtkComponent* arg0, unsigned int arg1)
{
  return atk_component_remove_focus_handler(arg0, arg1);
}

int Pure_atk_component_set_extents(AtkComponent* arg0, int arg1, int arg2, int arg3, int arg4, unsigned int arg5)
{
  return atk_component_set_extents(arg0, arg1, arg2, arg3, arg4, arg5);
}

int Pure_atk_component_set_position(AtkComponent* arg0, int arg1, int arg2, unsigned int arg3)
{
  return atk_component_set_position(arg0, arg1, arg2, arg3);
}

int Pure_atk_component_set_size(AtkComponent* arg0, int arg1, int arg2)
{
  return atk_component_set_size(arg0, arg1, arg2);
}

double Pure_atk_component_get_alpha(AtkComponent* arg0)
{
  return atk_component_get_alpha(arg0);
}

unsigned long Pure_atk_document_get_type()
{
  return atk_document_get_type();
}

char const* Pure_atk_document_get_document_type(AtkDocument* arg0)
{
  return atk_document_get_document_type(arg0);
}

void* Pure_atk_document_get_document(AtkDocument* arg0)
{
  return atk_document_get_document(arg0);
}

char const* Pure_atk_document_get_locale(AtkDocument* arg0)
{
  return atk_document_get_locale(arg0);
}

AtkAttributeSet* Pure_atk_document_get_attributes(AtkDocument* arg0)
{
  return atk_document_get_attributes(arg0);
}

char const* Pure_atk_document_get_attribute_value(AtkDocument* arg0, char const* arg1)
{
  return atk_document_get_attribute_value(arg0, arg1);
}

int Pure_atk_document_set_attribute_value(AtkDocument* arg0, char const* arg1, char const* arg2)
{
  return atk_document_set_attribute_value(arg0, arg1, arg2);
}

unsigned int Pure_atk_text_attribute_register(char const* arg0)
{
  return atk_text_attribute_register(arg0);
}

unsigned long Pure_atk_text_get_type()
{
  return atk_text_get_type();
}

char* Pure_atk_text_get_text(AtkText* arg0, int arg1, int arg2)
{
  return atk_text_get_text(arg0, arg1, arg2);
}

unsigned int Pure_atk_text_get_character_at_offset(AtkText* arg0, int arg1)
{
  return atk_text_get_character_at_offset(arg0, arg1);
}

char* Pure_atk_text_get_text_after_offset(AtkText* arg0, int arg1, unsigned int arg2, int* arg3, int* arg4)
{
  return atk_text_get_text_after_offset(arg0, arg1, arg2, arg3, arg4);
}

char* Pure_atk_text_get_text_at_offset(AtkText* arg0, int arg1, unsigned int arg2, int* arg3, int* arg4)
{
  return atk_text_get_text_at_offset(arg0, arg1, arg2, arg3, arg4);
}

char* Pure_atk_text_get_text_before_offset(AtkText* arg0, int arg1, unsigned int arg2, int* arg3, int* arg4)
{
  return atk_text_get_text_before_offset(arg0, arg1, arg2, arg3, arg4);
}

int Pure_atk_text_get_caret_offset(AtkText* arg0)
{
  return atk_text_get_caret_offset(arg0);
}

void Pure_atk_text_get_character_extents(AtkText* arg0, int arg1, int* arg2, int* arg3, int* arg4, int* arg5, unsigned int arg6)
{
  return atk_text_get_character_extents(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

AtkAttributeSet* Pure_atk_text_get_run_attributes(AtkText* arg0, int arg1, int* arg2, int* arg3)
{
  return atk_text_get_run_attributes(arg0, arg1, arg2, arg3);
}

AtkAttributeSet* Pure_atk_text_get_default_attributes(AtkText* arg0)
{
  return atk_text_get_default_attributes(arg0);
}

int Pure_atk_text_get_character_count(AtkText* arg0)
{
  return atk_text_get_character_count(arg0);
}

int Pure_atk_text_get_offset_at_point(AtkText* arg0, int arg1, int arg2, unsigned int arg3)
{
  return atk_text_get_offset_at_point(arg0, arg1, arg2, arg3);
}

int Pure_atk_text_get_n_selections(AtkText* arg0)
{
  return atk_text_get_n_selections(arg0);
}

char* Pure_atk_text_get_selection(AtkText* arg0, int arg1, int* arg2, int* arg3)
{
  return atk_text_get_selection(arg0, arg1, arg2, arg3);
}

int Pure_atk_text_add_selection(AtkText* arg0, int arg1, int arg2)
{
  return atk_text_add_selection(arg0, arg1, arg2);
}

int Pure_atk_text_remove_selection(AtkText* arg0, int arg1)
{
  return atk_text_remove_selection(arg0, arg1);
}

int Pure_atk_text_set_selection(AtkText* arg0, int arg1, int arg2, int arg3)
{
  return atk_text_set_selection(arg0, arg1, arg2, arg3);
}

int Pure_atk_text_set_caret_offset(AtkText* arg0, int arg1)
{
  return atk_text_set_caret_offset(arg0, arg1);
}

void Pure_atk_text_get_range_extents(AtkText* arg0, int arg1, int arg2, unsigned int arg3, AtkTextRectangle* arg4)
{
  return atk_text_get_range_extents(arg0, arg1, arg2, arg3, arg4);
}

AtkTextRange** Pure_atk_text_get_bounded_ranges(AtkText* arg0, AtkTextRectangle* arg1, unsigned int arg2, unsigned int arg3, unsigned int arg4)
{
  return atk_text_get_bounded_ranges(arg0, arg1, arg2, arg3, arg4);
}

void Pure_atk_text_free_ranges(AtkTextRange** arg0)
{
  return atk_text_free_ranges(arg0);
}

void Pure_atk_attribute_set_free(AtkAttributeSet* arg0)
{
  return atk_attribute_set_free(arg0);
}

char const* Pure_atk_text_attribute_get_name(unsigned int arg0)
{
  return atk_text_attribute_get_name(arg0);
}

unsigned int Pure_atk_text_attribute_for_name(char const* arg0)
{
  return atk_text_attribute_for_name(arg0);
}

char const* Pure_atk_text_attribute_get_value(unsigned int arg0, int arg1)
{
  return atk_text_attribute_get_value(arg0, arg1);
}

unsigned long Pure_atk_editable_text_get_type()
{
  return atk_editable_text_get_type();
}

int Pure_atk_editable_text_set_run_attributes(AtkEditableText* arg0, AtkAttributeSet* arg1, int arg2, int arg3)
{
  return atk_editable_text_set_run_attributes(arg0, arg1, arg2, arg3);
}

void Pure_atk_editable_text_set_text_contents(AtkEditableText* arg0, char const* arg1)
{
  return atk_editable_text_set_text_contents(arg0, arg1);
}

void Pure_atk_editable_text_insert_text(AtkEditableText* arg0, char const* arg1, int arg2, int* arg3)
{
  return atk_editable_text_insert_text(arg0, arg1, arg2, arg3);
}

void Pure_atk_editable_text_copy_text(AtkEditableText* arg0, int arg1, int arg2)
{
  return atk_editable_text_copy_text(arg0, arg1, arg2);
}

void Pure_atk_editable_text_cut_text(AtkEditableText* arg0, int arg1, int arg2)
{
  return atk_editable_text_cut_text(arg0, arg1, arg2);
}

void Pure_atk_editable_text_delete_text(AtkEditableText* arg0, int arg1, int arg2)
{
  return atk_editable_text_delete_text(arg0, arg1, arg2);
}

void Pure_atk_editable_text_paste_text(AtkEditableText* arg0, int arg1)
{
  return atk_editable_text_paste_text(arg0, arg1);
}

unsigned long Pure_atk_gobject_accessible_get_type()
{
  return atk_gobject_accessible_get_type();
}

AtkObject* Pure_atk_gobject_accessible_for_object(GObject* arg0)
{
  return atk_gobject_accessible_for_object(arg0);
}

GObject* Pure_atk_gobject_accessible_get_object(AtkGObjectAccessible* arg0)
{
  return atk_gobject_accessible_get_object(arg0);
}

unsigned long Pure_atk_hyperlink_get_type()
{
  return atk_hyperlink_get_type();
}

char* Pure_atk_hyperlink_get_uri(AtkHyperlink* arg0, int arg1)
{
  return atk_hyperlink_get_uri(arg0, arg1);
}

AtkObject* Pure_atk_hyperlink_get_object(AtkHyperlink* arg0, int arg1)
{
  return atk_hyperlink_get_object(arg0, arg1);
}

int Pure_atk_hyperlink_get_end_index(AtkHyperlink* arg0)
{
  return atk_hyperlink_get_end_index(arg0);
}

int Pure_atk_hyperlink_get_start_index(AtkHyperlink* arg0)
{
  return atk_hyperlink_get_start_index(arg0);
}

int Pure_atk_hyperlink_is_valid(AtkHyperlink* arg0)
{
  return atk_hyperlink_is_valid(arg0);
}

int Pure_atk_hyperlink_is_inline(AtkHyperlink* arg0)
{
  return atk_hyperlink_is_inline(arg0);
}

int Pure_atk_hyperlink_get_n_anchors(AtkHyperlink* arg0)
{
  return atk_hyperlink_get_n_anchors(arg0);
}

int Pure_atk_hyperlink_is_selected_link(AtkHyperlink* arg0)
{
  return atk_hyperlink_is_selected_link(arg0);
}

unsigned long Pure_atk_hyperlink_impl_get_type()
{
  return atk_hyperlink_impl_get_type();
}

AtkHyperlink* Pure_atk_hyperlink_impl_get_hyperlink(AtkHyperlinkImpl* arg0)
{
  return atk_hyperlink_impl_get_hyperlink(arg0);
}

unsigned long Pure_atk_hypertext_get_type()
{
  return atk_hypertext_get_type();
}

AtkHyperlink* Pure_atk_hypertext_get_link(AtkHypertext* arg0, int arg1)
{
  return atk_hypertext_get_link(arg0, arg1);
}

int Pure_atk_hypertext_get_n_links(AtkHypertext* arg0)
{
  return atk_hypertext_get_n_links(arg0);
}

int Pure_atk_hypertext_get_link_index(AtkHypertext* arg0, int arg1)
{
  return atk_hypertext_get_link_index(arg0, arg1);
}

unsigned long Pure_atk_image_get_type()
{
  return atk_image_get_type();
}

char const* Pure_atk_image_get_image_description(AtkImage* arg0)
{
  return atk_image_get_image_description(arg0);
}

void Pure_atk_image_get_image_size(AtkImage* arg0, int* arg1, int* arg2)
{
  return atk_image_get_image_size(arg0, arg1, arg2);
}

int Pure_atk_image_set_image_description(AtkImage* arg0, char const* arg1)
{
  return atk_image_set_image_description(arg0, arg1);
}

void Pure_atk_image_get_image_position(AtkImage* arg0, int* arg1, int* arg2, unsigned int arg3)
{
  return atk_image_get_image_position(arg0, arg1, arg2, arg3);
}

char const* Pure_atk_image_get_image_locale(AtkImage* arg0)
{
  return atk_image_get_image_locale(arg0);
}

unsigned long Pure_atk_no_op_object_get_type()
{
  return atk_no_op_object_get_type();
}

AtkObject* Pure_atk_no_op_object_new(GObject* arg0)
{
  return atk_no_op_object_new(arg0);
}

unsigned long Pure_atk_object_factory_get_type()
{
  return atk_object_factory_get_type();
}

AtkObject* Pure_atk_object_factory_create_accessible(AtkObjectFactory* arg0, GObject* arg1)
{
  return atk_object_factory_create_accessible(arg0, arg1);
}

void Pure_atk_object_factory_invalidate(AtkObjectFactory* arg0)
{
  return atk_object_factory_invalidate(arg0);
}

unsigned long Pure_atk_object_factory_get_accessible_type(AtkObjectFactory* arg0)
{
  return atk_object_factory_get_accessible_type(arg0);
}

unsigned long Pure_atk_no_op_object_factory_get_type()
{
  return atk_no_op_object_factory_get_type();
}

AtkObjectFactory* Pure_atk_no_op_object_factory_new()
{
  return atk_no_op_object_factory_new();
}

unsigned long Pure_atk_registry_get_type()
{
  return atk_registry_get_type();
}

void Pure_atk_registry_set_factory_type(AtkRegistry* arg0, unsigned long arg1, unsigned long arg2)
{
  return atk_registry_set_factory_type(arg0, arg1, arg2);
}

unsigned long Pure_atk_registry_get_factory_type(AtkRegistry* arg0, unsigned long arg1)
{
  return atk_registry_get_factory_type(arg0, arg1);
}

AtkObjectFactory* Pure_atk_registry_get_factory(AtkRegistry* arg0, unsigned long arg1)
{
  return atk_registry_get_factory(arg0, arg1);
}

AtkRegistry* Pure_atk_get_default_registry()
{
  return atk_get_default_registry();
}

unsigned long Pure_atk_relation_get_type()
{
  return atk_relation_get_type();
}

unsigned int Pure_atk_relation_type_register(char const* arg0)
{
  return atk_relation_type_register(arg0);
}

char const* Pure_atk_relation_type_get_name(unsigned int arg0)
{
  return atk_relation_type_get_name(arg0);
}

unsigned int Pure_atk_relation_type_for_name(char const* arg0)
{
  return atk_relation_type_for_name(arg0);
}

AtkRelation* Pure_atk_relation_new(AtkObject** arg0, int arg1, unsigned int arg2)
{
  return atk_relation_new(arg0, arg1, arg2);
}

unsigned int Pure_atk_relation_get_relation_type(AtkRelation* arg0)
{
  return atk_relation_get_relation_type(arg0);
}

GPtrArray* Pure_atk_relation_get_target(AtkRelation* arg0)
{
  return atk_relation_get_target(arg0);
}

void Pure_atk_relation_add_target(AtkRelation* arg0, AtkObject* arg1)
{
  return atk_relation_add_target(arg0, arg1);
}

unsigned long Pure_atk_relation_set_get_type()
{
  return atk_relation_set_get_type();
}

AtkRelationSet* Pure_atk_relation_set_new()
{
  return atk_relation_set_new();
}

int Pure_atk_relation_set_contains(AtkRelationSet* arg0, unsigned int arg1)
{
  return atk_relation_set_contains(arg0, arg1);
}

void Pure_atk_relation_set_remove(AtkRelationSet* arg0, AtkRelation* arg1)
{
  return atk_relation_set_remove(arg0, arg1);
}

void Pure_atk_relation_set_add(AtkRelationSet* arg0, AtkRelation* arg1)
{
  return atk_relation_set_add(arg0, arg1);
}

int Pure_atk_relation_set_get_n_relations(AtkRelationSet* arg0)
{
  return atk_relation_set_get_n_relations(arg0);
}

AtkRelation* Pure_atk_relation_set_get_relation(AtkRelationSet* arg0, int arg1)
{
  return atk_relation_set_get_relation(arg0, arg1);
}

AtkRelation* Pure_atk_relation_set_get_relation_by_type(AtkRelationSet* arg0, unsigned int arg1)
{
  return atk_relation_set_get_relation_by_type(arg0, arg1);
}

void Pure_atk_relation_set_add_relation_by_type(AtkRelationSet* arg0, unsigned int arg1, AtkObject* arg2)
{
  return atk_relation_set_add_relation_by_type(arg0, arg1, arg2);
}

unsigned long Pure_atk_selection_get_type()
{
  return atk_selection_get_type();
}

int Pure_atk_selection_add_selection(AtkSelection* arg0, int arg1)
{
  return atk_selection_add_selection(arg0, arg1);
}

int Pure_atk_selection_clear_selection(AtkSelection* arg0)
{
  return atk_selection_clear_selection(arg0);
}

AtkObject* Pure_atk_selection_ref_selection(AtkSelection* arg0, int arg1)
{
  return atk_selection_ref_selection(arg0, arg1);
}

int Pure_atk_selection_get_selection_count(AtkSelection* arg0)
{
  return atk_selection_get_selection_count(arg0);
}

int Pure_atk_selection_is_child_selected(AtkSelection* arg0, int arg1)
{
  return atk_selection_is_child_selected(arg0, arg1);
}

int Pure_atk_selection_remove_selection(AtkSelection* arg0, int arg1)
{
  return atk_selection_remove_selection(arg0, arg1);
}

int Pure_atk_selection_select_all_selection(AtkSelection* arg0)
{
  return atk_selection_select_all_selection(arg0);
}

unsigned long Pure_atk_state_set_get_type()
{
  return atk_state_set_get_type();
}

AtkStateSet* Pure_atk_state_set_new()
{
  return atk_state_set_new();
}

int Pure_atk_state_set_is_empty(AtkStateSet* arg0)
{
  return atk_state_set_is_empty(arg0);
}

int Pure_atk_state_set_add_state(AtkStateSet* arg0, unsigned int arg1)
{
  return atk_state_set_add_state(arg0, arg1);
}

void Pure_atk_state_set_add_states(AtkStateSet* arg0, unsigned int* arg1, int arg2)
{
  return atk_state_set_add_states(arg0, arg1, arg2);
}

void Pure_atk_state_set_clear_states(AtkStateSet* arg0)
{
  return atk_state_set_clear_states(arg0);
}

int Pure_atk_state_set_contains_state(AtkStateSet* arg0, unsigned int arg1)
{
  return atk_state_set_contains_state(arg0, arg1);
}

int Pure_atk_state_set_contains_states(AtkStateSet* arg0, unsigned int* arg1, int arg2)
{
  return atk_state_set_contains_states(arg0, arg1, arg2);
}

int Pure_atk_state_set_remove_state(AtkStateSet* arg0, unsigned int arg1)
{
  return atk_state_set_remove_state(arg0, arg1);
}

AtkStateSet* Pure_atk_state_set_and_sets(AtkStateSet* arg0, AtkStateSet* arg1)
{
  return atk_state_set_and_sets(arg0, arg1);
}

AtkStateSet* Pure_atk_state_set_or_sets(AtkStateSet* arg0, AtkStateSet* arg1)
{
  return atk_state_set_or_sets(arg0, arg1);
}

AtkStateSet* Pure_atk_state_set_xor_sets(AtkStateSet* arg0, AtkStateSet* arg1)
{
  return atk_state_set_xor_sets(arg0, arg1);
}

unsigned long Pure_atk_streamable_content_get_type()
{
  return atk_streamable_content_get_type();
}

int Pure_atk_streamable_content_get_n_mime_types(AtkStreamableContent* arg0)
{
  return atk_streamable_content_get_n_mime_types(arg0);
}

char const* Pure_atk_streamable_content_get_mime_type(AtkStreamableContent* arg0, int arg1)
{
  return atk_streamable_content_get_mime_type(arg0, arg1);
}

GIOChannel* Pure_atk_streamable_content_get_stream(AtkStreamableContent* arg0, char const* arg1)
{
  return atk_streamable_content_get_stream(arg0, arg1);
}

char* Pure_atk_streamable_content_get_uri(AtkStreamableContent* arg0, char const* arg1)
{
  return atk_streamable_content_get_uri(arg0, arg1);
}

unsigned long Pure_atk_table_get_type()
{
  return atk_table_get_type();
}

AtkObject* Pure_atk_table_ref_at(AtkTable* arg0, int arg1, int arg2)
{
  return atk_table_ref_at(arg0, arg1, arg2);
}

int Pure_atk_table_get_index_at(AtkTable* arg0, int arg1, int arg2)
{
  return atk_table_get_index_at(arg0, arg1, arg2);
}

int Pure_atk_table_get_column_at_index(AtkTable* arg0, int arg1)
{
  return atk_table_get_column_at_index(arg0, arg1);
}

int Pure_atk_table_get_row_at_index(AtkTable* arg0, int arg1)
{
  return atk_table_get_row_at_index(arg0, arg1);
}

int Pure_atk_table_get_n_columns(AtkTable* arg0)
{
  return atk_table_get_n_columns(arg0);
}

int Pure_atk_table_get_n_rows(AtkTable* arg0)
{
  return atk_table_get_n_rows(arg0);
}

int Pure_atk_table_get_column_extent_at(AtkTable* arg0, int arg1, int arg2)
{
  return atk_table_get_column_extent_at(arg0, arg1, arg2);
}

int Pure_atk_table_get_row_extent_at(AtkTable* arg0, int arg1, int arg2)
{
  return atk_table_get_row_extent_at(arg0, arg1, arg2);
}

AtkObject* Pure_atk_table_get_caption(AtkTable* arg0)
{
  return atk_table_get_caption(arg0);
}

char const* Pure_atk_table_get_column_description(AtkTable* arg0, int arg1)
{
  return atk_table_get_column_description(arg0, arg1);
}

AtkObject* Pure_atk_table_get_column_header(AtkTable* arg0, int arg1)
{
  return atk_table_get_column_header(arg0, arg1);
}

char const* Pure_atk_table_get_row_description(AtkTable* arg0, int arg1)
{
  return atk_table_get_row_description(arg0, arg1);
}

AtkObject* Pure_atk_table_get_row_header(AtkTable* arg0, int arg1)
{
  return atk_table_get_row_header(arg0, arg1);
}

AtkObject* Pure_atk_table_get_summary(AtkTable* arg0)
{
  return atk_table_get_summary(arg0);
}

void Pure_atk_table_set_caption(AtkTable* arg0, AtkObject* arg1)
{
  return atk_table_set_caption(arg0, arg1);
}

void Pure_atk_table_set_column_description(AtkTable* arg0, int arg1, char const* arg2)
{
  return atk_table_set_column_description(arg0, arg1, arg2);
}

void Pure_atk_table_set_column_header(AtkTable* arg0, int arg1, AtkObject* arg2)
{
  return atk_table_set_column_header(arg0, arg1, arg2);
}

void Pure_atk_table_set_row_description(AtkTable* arg0, int arg1, char const* arg2)
{
  return atk_table_set_row_description(arg0, arg1, arg2);
}

void Pure_atk_table_set_row_header(AtkTable* arg0, int arg1, AtkObject* arg2)
{
  return atk_table_set_row_header(arg0, arg1, arg2);
}

void Pure_atk_table_set_summary(AtkTable* arg0, AtkObject* arg1)
{
  return atk_table_set_summary(arg0, arg1);
}

int Pure_atk_table_get_selected_columns(AtkTable* arg0, int** arg1)
{
  return atk_table_get_selected_columns(arg0, arg1);
}

int Pure_atk_table_get_selected_rows(AtkTable* arg0, int** arg1)
{
  return atk_table_get_selected_rows(arg0, arg1);
}

int Pure_atk_table_is_column_selected(AtkTable* arg0, int arg1)
{
  return atk_table_is_column_selected(arg0, arg1);
}

int Pure_atk_table_is_row_selected(AtkTable* arg0, int arg1)
{
  return atk_table_is_row_selected(arg0, arg1);
}

int Pure_atk_table_is_selected(AtkTable* arg0, int arg1, int arg2)
{
  return atk_table_is_selected(arg0, arg1, arg2);
}

int Pure_atk_table_add_row_selection(AtkTable* arg0, int arg1)
{
  return atk_table_add_row_selection(arg0, arg1);
}

int Pure_atk_table_remove_row_selection(AtkTable* arg0, int arg1)
{
  return atk_table_remove_row_selection(arg0, arg1);
}

int Pure_atk_table_add_column_selection(AtkTable* arg0, int arg1)
{
  return atk_table_add_column_selection(arg0, arg1);
}

int Pure_atk_table_remove_column_selection(AtkTable* arg0, int arg1)
{
  return atk_table_remove_column_selection(arg0, arg1);
}

unsigned long Pure_atk_misc_get_type()
{
  return atk_misc_get_type();
}

void Pure_atk_misc_threads_enter(AtkMisc* arg0)
{
  return atk_misc_threads_enter(arg0);
}

void Pure_atk_misc_threads_leave(AtkMisc* arg0)
{
  return atk_misc_threads_leave(arg0);
}

AtkMisc const* Pure_atk_misc_get_instance()
{
  return atk_misc_get_instance();
}

unsigned long Pure_atk_value_get_type()
{
  return atk_value_get_type();
}

void Pure_atk_value_get_current_value(AtkValue* arg0, GValue* arg1)
{
  return atk_value_get_current_value(arg0, arg1);
}

void Pure_atk_value_get_maximum_value(AtkValue* arg0, GValue* arg1)
{
  return atk_value_get_maximum_value(arg0, arg1);
}

void Pure_atk_value_get_minimum_value(AtkValue* arg0, GValue* arg1)
{
  return atk_value_get_minimum_value(arg0, arg1);
}

int Pure_atk_value_set_current_value(AtkValue* arg0, GValue const* arg1)
{
  return atk_value_set_current_value(arg0, arg1);
}

void Pure_atk_value_get_minimum_increment(AtkValue* arg0, GValue* arg1)
{
  return atk_value_get_minimum_increment(arg0, arg1);
}
