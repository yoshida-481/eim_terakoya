CREATE OR REPLACE PROCEDURE init_belonging_workspace
IS
    type oid_array is TABLE of INTEGER(32);
    
    i_ws_oid_list           oid_array;
    i_doc_child_list        oid_array;

    ws_obj_type             INTEGER(32);
    
    doc_rel_type            INTEGER(32);

BEGIN
    
    -- 属性を取得
    select id into ws_obj_type from EIMOBJTYPE where name = 'ワークスペース';
    select id into doc_rel_type from EIMRELTYPE where name  = 'ドキュメント';
    
    -- ワークスペースを取得する
    select id bulk collect into i_ws_oid_list from EIMOBJ where type = ws_obj_type;
    
    for ws_count in 1..i_ws_oid_list.count loop
      -- フォルダ、タグ、ドキュメントの子オブジェクトを取得する
      select child bulk collect into i_doc_child_list from EIMREL where parent = i_ws_oid_list(ws_count) and type = doc_rel_type;
      
      for child_count in 1..i_doc_child_list.count loop
        -- 登録プロシージャをドキュメントで呼び出す
        -- 所属ワークスペース属性の値を設定する
        set_belonging_workspace(i_doc_child_list(child_count), i_ws_oid_list(ws_count), 0);
        -- 過去のドキュメントに対しても所属ワークスペース属性の値を設定する
        set_value_previous_doc(i_doc_child_list(child_count), i_ws_oid_list(ws_count));
      end loop;
    end loop;

    commit;

END;
/

