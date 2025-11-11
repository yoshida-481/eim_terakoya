CREATE OR REPLACE PROCEDURE delete_belonging_workspace(
                            child_oid         IN INTEGER,
                            workspace_id      IN INTEGER,
                            parent_oid        IN INTEGER
                            )
IS
    i_count_overlap         INTEGER(32);
    
    i_count_link            INTEGER(32);
    
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
    
    -- 対象がオブジェクトかリンクか
    if parent_oid = 0 then
      -- ドキュメントの場合
      -- 所属ワークスペースを両方消す
      delete EIMOBJINT where id = child_oid and type in (ws_attr_type, link_ws_attr_type);
      
      -- ドキュメントリレーションの子オブジェクトから所属ワークスペースを両方消す
      delete EIMOBJINT where type in (ws_attr_type, link_ws_attr_type) and 
                  id in (select distinct child from EIMREL where type = doc_rel_type and
                                id in (select id from EIMREL start with parent = child_oid connect by prior child = parent)
                        );
      
      -- リンクリレーションの子オブジェクトを全て取得する
      select distinct child bulk collect into target_obj_list_link from EIMREL where type = link_rel_type and
             id in (select id from EIMREL start with parent = child_oid connect by prior child = parent);
      
      for i_count_l_target in 1..target_obj_list_link.count loop
        -- リンク所属ワークスペースに値がない場合は何もしない
        select count(id) into i_count_link from EIMOBJINT where id = target_obj_list_link(i_count_l_target) and type = link_ws_attr_type and value = workspace_id;
        
        if i_count_link > 0 then
          -- 親オブジェクトに同じ値が含まれていない場合に削除
          select count(id) into i_count_overlap from EIMOBJINT where
                           id in (select parent from EIMREL where child = target_obj_list_link(i_count_link) and type = link_rel_type) and
                           value = workspace_id and
                           type = link_ws_attr_type;
          
          if i_count_overlap = 0 then
            delete from EIMOBJINT where id = target_obj_list_link(i_count_link) and type = link_ws_attr_type and value = workspace_id;
          end if;
        end if;
      end loop;
      
    else
      select count(id) into i_count_link from EIMOBJINT where id = child_oid and type = link_ws_attr_type and value = workspace_id;
      if i_count_link > 0 then
        -- リンクの場合
        -- 親オブジェクトに同じ値が含まれていない場合に削除
        select count(id) into i_count_overlap from EIMOBJINT where
                         id in (select parent from EIMREL where child = child_oid and type = link_rel_type) and
                         value = workspace_id and
                         type = link_ws_attr_type;
        if i_count_overlap = 0 then
          delete from EIMOBJINT where id = child_oid and type = link_ws_attr_type and value = workspace_id;
        end if;
      end if;
      
    end if;

END;
/

