drop trigger update_workspace_trigger;
drop trigger delete_workspace_trigger;
drop trigger insert_workspace_trigger;
commit;

@install_temptable.sql
@insert_workspace_trg_before.sql
@insert_workspace_trg_after.sql
@update_workspace_trg_before.sql
@update_workspace_trg_after.sql
@delete_workspace_trg_before.sql
@delete_workspace_trg_after.sql
