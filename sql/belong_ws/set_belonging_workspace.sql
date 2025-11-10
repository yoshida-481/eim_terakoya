CREATE OR REPLACE PROCEDURE set_belonging_workspace(
                            child_oid IN INTEGER,
                            workspace_doc IN INTEGER,
                            workspace_link IN INTEGER
                            )
IS
    i_old_workspace_doc     INTEGER(32);

    i_count_overlap         INTEGER(32);
    i_count_link_item       INTEGER(32);
    i_count_max             INTEGER(32);

    ws_attr_type            INTEGER(32);
    link_ws_attr_type       INTEGER(32);

    doc_rel_type            INTEGER(32);
    link_rel_type           INTEGER(32);

    type oid_array is TABLE of INTEGER(32);

    target_obj_list         oid_array;
    target_obj_list_link    oid_array;

BEGIN
    -- 属性を取得
    select id into ws_attr_type from EIMATTR where name = '所属ワークスペース';
    select id into link_ws_attr_type from EIMATTR where name = 'リンク所属ワークスペース';
    select id into doc_rel_type from EIMRELTYPE where name  = 'ドキュメント';
    select id into link_rel_type from EIMRELTYPE where name = 'リンク';

    -- 引数2が0の場合はドキュメント用所属ワークスペースは何もしない
    if workspace_doc != 0 then
      -- 引数1のオブジェクトのドキュメント用所属ワークスペースを取得
      select count(value) into i_old_workspace_doc from EIMOBJINT where id = child_oid and type = ws_attr_type;
      -- 引数1の所属ワークスペースを設定
      if i_old_workspace_doc = 0 then
        -- 未設定の場合はInsert
        insert into EIMOBJINT values(child_oid, ws_attr_type, 0, workspace_doc);
      else
        -- 値が既に入ってる場合はUpdate
        update EIMOBJINT set value = workspace_doc where id = child_oid and type = ws_attr_type;
      end if;
      select count(id) into i_old_workspace_doc from EIMREL where type = doc_rel_type and
             id in (select id from EIMREL start with parent = child_oid connect by prior child = parent);
      if i_old_workspace_doc != 0 then
        -- 所属ワークスペース設定対象のIDの一覧を取得する
        select distinct child bulk collect into target_obj_list from EIMREL where type = doc_rel_type and
               id in (select id from EIMREL start with parent = child_oid connect by prior child = parent);
        -- 子オブジェクト達の所属ワークスペースを設定する
        for i_count_target in 1..target_obj_list.count loop
          select count(value) into i_old_workspace_doc from EIMOBJINT where id = target_obj_list(i_count_target) and type = ws_attr_type;
          if i_old_workspace_doc = 0 then
            -- 未設定の場合はInsert
            insert into EIMOBJINT values(target_obj_list(i_count_target), ws_attr_type, 0, workspace_doc);
          else
            -- 値が既に入ってる場合はUpdate
            update EIMOBJINT set value  = workspace_doc where id = target_obj_list(i_count_target) and type = ws_attr_type;
          end if;
        end loop;
      end if;

      select count(id) into i_old_workspace_doc from EIMREL where type = link_rel_type and
             id in (select id from EIMREL start with parent = child_oid connect by prior child = parent);
      if i_old_workspace_doc != 0 then
        -- リンク所属ワークスペース設定対象のIDの一覧を取得する
        select distinct child bulk collect into target_obj_list_link from EIMREL where type = link_rel_type and
               id in (select id from EIMREL start with parent = child_oid connect by prior child = parent);
        -- 子オブジェクト達のリンク所属ワークスペースを設定する
        for i_count_l_target in 1..target_obj_list_link.count loop
          -- 重複を確認
          i_count_overlap := 0;
          select count(id) into i_count_overlap from EIMOBJINT where id = target_obj_list_link(i_count_l_target) and type = link_ws_attr_type and value = workspace_doc;
          if i_count_overlap = 0 then
            i_count_link_item := 0;
            select count(id) into i_count_link_item from EIMOBJINT where id = target_obj_list_link(i_count_l_target) and type = link_ws_attr_type;
            if i_count_link_item = 0 then
              -- 未設定の場合はkeyを1でInsert
              insert into EIMOBJINT values(target_obj_list_link(i_count_l_target), link_ws_attr_type, 1, workspace_doc);
            else
              -- 値が既に入ってる場合は最大値+1で設定
              i_count_max := 0;
              select max(key) into i_count_max from EIMOBJINT where id = target_obj_list_link(i_count_l_target) and type = link_ws_attr_type;
              i_count_max := i_count_max + 1;
              insert into EIMOBJINT values(target_obj_list_link(i_count_l_target), link_ws_attr_type, i_count_max, workspace_doc);
            end if;
          end if;
        end loop;
      end if;

    end if;

    -- 引数3の値が0でない場合
    if workspace_link != 0 then
      -- 重複を確認
      select count(id) into i_count_overlap from EIMOBJINT where id = child_oid and type = link_ws_attr_type and value = workspace_link;
      if i_count_overlap = 0 then
        -- 重複してない場合は登録する
        select count(id) into i_count_link_item from EIMOBJINT where id = child_oid and type = link_ws_attr_type;
        if i_count_link_item = 0 then
          insert into EIMOBJINT values(child_oid, link_ws_attr_type, 1, workspace_link);
        else
          select max(key) into i_count_max from EIMOBJINT where id = child_oid and type = link_ws_attr_type;
          insert into EIMOBJINT values(child_oid, link_ws_attr_type, i_count_max + 1, workspace_link);
        end if;
      end if;

    end if;

END;
/
