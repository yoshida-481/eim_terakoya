CREATE OR REPLACE TRIGGER update_workspace_trg_after
                  AFTER UPDATE
                  ON EIMREL
DECLARE

	cursor cur_reltmp(I_ws_obj_type number, I_ws_attr_type number) is
	select
		nvl(case 
				when old_ws_obj.id is not null
				then old_ws_obj.id
				else old_objint.value
			end, 0) old_ws_oid,
		nvl(case 
				when new_ws_obj.id is not null
				then new_ws_obj.id
				else new_objint.value
			end, 0) new_ws_oid,
		new_r.id id,
		new_r.type new_type,
		new_r.parent new_parent,
		new_r.child new_child,
		old_r.type old_type,
		old_r.parent old_parent,
		old_r.child old_child
	from
		EIMRELTMP old_r,
		EIMREL new_r,
		EIMOBJ old_ws_obj,
		EIMOBJ new_ws_obj,
		EIMOBJINT old_objint,
		EIMOBJINT new_objint
	where
		old_r.id = new_r.id
	and old_ws_obj.id (+) = old_r.parent and old_ws_obj.type (+) = I_ws_obj_type
	and new_ws_obj.id (+) = new_r.parent and new_ws_obj.type (+) = I_ws_obj_type
	and old_objint.id (+) = old_r.parent and old_objint.type (+) = I_ws_attr_type
	and new_objint.id (+) = new_r.parent and new_objint.type (+) = I_ws_attr_type
	and (old_ws_obj.id is not null or new_ws_obj.id is not null or old_objint.id is not null or new_objint.id is not null)
	;

	ws_obj_type		number(32);
	ws_attr_type	number(32);

	doc_rel_type	number(32);
	link_rel_type	number(32);

BEGIN

    -- 属性を取得
    select id into ws_obj_type from EIMOBJTYPE where name = 'ワークスペース';
    select id into ws_attr_type from EIMATTR where name = '所属ワークスペース';
    select id into doc_rel_type from EIMRELTYPE where name  = 'ドキュメント';
    select id into link_rel_type from EIMRELTYPE where name = 'リンク';

	FOR r IN cur_reltmp(ws_obj_type, ws_attr_type) LOOP

		if r.old_ws_oid != 0 then

			-- タイプがリンクかドキュメントかで分岐
			if r.old_type = doc_rel_type then
				-- ドキュメントの場合は削除プロシージャをドキュメント用で呼び出す
				delete_belonging_workspace(r.old_child, r.old_ws_oid, 0);

			elsif r.old_type = link_rel_type then
				-- リンクの場合は削除プロシージャをリンク用で呼び出す
				delete_belonging_workspace(r.old_child, r.old_ws_oid, r.old_parent);

			end if;

		end if;

		if r.new_ws_oid != 0 then

			-- タイプがリンクかドキュメントかで分岐
			if r.new_type = doc_rel_type then
				-- ドキュメントの場合は登録プロシージャをドキュメント用で呼び出す
				set_belonging_workspace(r.new_child, r.new_ws_oid, 0);

			elsif r.new_type = link_rel_type then
				-- リンクの場合は登録プロシージャをリンク用で呼び出す
				set_belonging_workspace(r.new_child, 0, r.new_ws_oid);

			end if;

		end if;

		delete from EIMRELTMP where id = r.id;

	END LOOP;

EXCEPTION
    when others then
      return;
END;
/

