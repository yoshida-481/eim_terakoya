CREATE OR REPLACE PROCEDURE set_value_previous_doc(
                            I_objId IN INTEGER,
                            I_workspaceId IN INTEGER
                            )

IS
    ws_attr_type            INTEGER(32);
    doc_rel_type            INTEGER(32);

    i_exist_version         INTEGER(32);
    i_versionId             INTEGER(32);
    i_doc_length            INTEGER(32);
    i_old_workspace         INTEGER(32);
    i_child_count           INTEGER(32);

    type oid_array is TABLE of INTEGER(32);

    i_doc_list              oid_array;
    i_target_obj_list       oid_array;

BEGIN
    -- 属性を取得
    select id into ws_attr_type from EIMATTR where name = '所属ワークスペース';
    select id into doc_rel_type from EIMRELTYPE where name = 'ドキュメント';

    -- 指定ワークスペースの値が0の場合は何もしない
    if I_workspaceId != 0 then
        -- I_objIdに対応するバージョンが存在するかチェック
        select count(*) into i_exist_version from EIMVER where oid = I_objId;
        if i_exist_version != 0 then
            -- I_objIdから対応するバージョン取得
            select vid into i_versionId from EIMVER where oid = I_objId;
            -- リビジョン管理されているオブジェクトが処理対象(バージョン内にオブジェクト数が2以上のもの)
            select count(*) into i_doc_length from EIMOBJ where id in (select oid from EIMVER where vid = i_versionId);
            if i_doc_length > 1 then
                -- バージョン内ドキュメント一覧取得
                select id bulk collect into i_doc_list from EIMOBJ where id in (select oid from EIMVER where vid = i_versionId);
                for doc_count in 1..i_doc_list.count loop
                    -- 指定の過去リビジョンのオブジェクトに所属ワークスペースの値が設定されているかチェック
                    select count(value) into i_old_workspace from EIMOBJINT where id = i_doc_list(doc_count) and type = ws_attr_type;
                    if i_old_workspace = 0 then
                        -- 未設定の場合はInsert
                        insert into EIMOBJINT values(i_doc_list(doc_count), ws_attr_type, 0, I_workspaceId);
                    else
                        -- 値が既に設定されている場合はUpdate
                        update EIMOBJINT set value = I_workspaceId where id = i_doc_list(doc_count) and type = ws_attr_type;
                    end if;
                end loop;
            end if;
        end if;
        
        -- 指定オブジェクトの子階層のオブジェクト数取得
        select count(id) into i_child_count from EIMREL where type = doc_rel_type and
            id in (select id from EIMREL start with parent = I_objId connect by prior child = parent);
        if i_child_count != 0 then
            -- 所属ワークスペース設定対象のID一覧を取得
            select distinct child bulk collect into i_target_obj_list from EIMREL where type = doc_rel_type and
                   id in (select id from EIMREL start with parent = I_objId connect by prior child = parent);
            -- 子オブジェクトリストに所属ワークスペースの値を設定する
            for target_count in 1..i_target_obj_list.count loop
                -- 処理対象オブジェクトに対応するバージョンが存在するかチェック
                select count(*) into i_exist_version from EIMVER where oid = i_target_obj_list(target_count);
                if i_exist_version != 0 then
                    -- 処理対象オブジェクトに対応するバージョン取得
                    select vid into i_versionId from EIMVER where oid = i_target_obj_list(target_count);
                    -- リビジョン管理されているオブジェクトが処理対象(バージョン内にオブジェクト数が2以上のもの)
                    select count(*) into i_doc_length from EIMOBJ where id in (select oid from EIMVER where vid = i_versionId);
                    if i_doc_length > 1 then
                        -- バージョン内ドキュメント一覧取得
                        select id bulk collect into i_doc_list from EIMOBJ where id in (select oid from EIMVER where vid = i_versionId);
                        for doc_count in 1..i_doc_list.count loop
                            -- 指定の過去リビジョンのオブジェクトに所属ワークスペースの値が設定されているかチェック
                            select count(value) into i_old_workspace from EIMOBJINT where id = i_doc_list(doc_count) and type = ws_attr_type;
                            if i_old_workspace = 0 then
                                -- 未設定の場合はInsert
                                insert into EIMOBJINT values(i_doc_list(doc_count), ws_attr_type, 0, I_workspaceId);
                            else
                                -- 値が既に設定されている場合はUpdate
                                update EIMOBJINT set value = I_workspaceId where id = i_doc_list(doc_count) and type = ws_attr_type;
                            end if;
                        end loop;
                    end if;
                end if;
            end loop;
        end if;
    end if;
END;
/
