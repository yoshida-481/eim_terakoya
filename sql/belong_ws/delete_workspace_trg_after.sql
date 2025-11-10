CREATE OR REPLACE TRIGGER delete_workspace_trg_after
                  AFTER DELETE
                  ON EIMREL
DECLARE

	cursor cur_reltmp(I_ws_obj_type number, I_ws_attr_type number) is
	select
		obj.id ws_oid,
		r.id,
		r.type,
		r.parent,
		r.child
	from
		EIMRELTMP r,
		EIMOBJ obj
	where
		obj.id = r.parent and obj.type = I_ws_obj_type
	union all
	select
		objint.value ws_oid,
		r.id,
		r.type,
		r.parent,
		r.child
	from
		EIMRELTMP r,
		EIMOBJINT objint
	where
		objint.id = r.parent and objint.type = I_ws_attr_type
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

		-- タイプがリンクかドキュメントかで分岐
		if r.type = doc_rel_type then
			-- ドキュメントの場合は削除プロシージャをドキュメント用で呼び出す
			delete_belonging_workspace(r.child, r.ws_oid, 0);

		elsif r.type = link_rel_type then
			-- リンクの場合は削除プロシージャをリンク用で呼び出す
			delete_belonging_workspace(r.child, r.ws_oid, r.parent);

		end if;

		delete from EIMRELTMP where id = r.id;

    END LOOP;

EXCEPTION
    when others then
      return;
END;
/