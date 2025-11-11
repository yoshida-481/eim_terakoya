CREATE OR REPLACE TRIGGER update_workspace_trg_before
                  BEFORE UPDATE
                  ON EIMREL
                  FOR EACH ROW
DECLARE

	doc_rel_type			number(32);
	link_rel_type			number(32);

	ws_obj_type				number(32);
	ws_attr_type            number(32);

	data_count				number(32);

BEGIN

	--
	-- 文書管理のリレーション("ドキュメント","リンク")でない場合処理対象外
	--
	select id into doc_rel_type from EIMRELTYPE where name  = 'ドキュメント';
	select id into link_rel_type from EIMRELTYPE where name = 'リンク';

	if :old.type != doc_rel_type and :old.type != link_rel_type
		and :new.type != doc_rel_type and :new.type != link_rel_type then

		return;

	end if;


	--
	-- リレーションの親オブジェクトがワークスペースの場合処理対象
	--
	select id into ws_obj_type from EIMOBJTYPE where name = 'ワークスペース';
	select count(id) into data_count from EIMOBJ where id in (:old.parent, :new.parent) and type = ws_obj_type;

	if data_count > 0 then

		-- 一時表に更新対象データを書き込む。（AFTERトリガ（文トリガ）でこのデータを参照して処理する。）
		insert into EIMRELTMP values(:old.id, :old.type, :old.parent, :old.child);
		return;

	end if;


	--
	-- リレーションの親オブジェクトが所属ワークスペース属性を保持する場合処理対象
	--
	select id into ws_attr_type from EIMATTR where name = '所属ワークスペース';
	select count(value) into data_count from EIMOBJINT where id in (:old.parent, :new.parent) and type = ws_attr_type;

	if data_count > 0 then

		-- 一時表に更新対象データを書き込む。（AFTERトリガ（文トリガ）でこのデータを参照して処理する。）
		insert into EIMRELTMP values(:old.id, :old.type, :old.parent, :old.child);
		return;

	end if;

EXCEPTION
    when others then
      return;
END;
/

