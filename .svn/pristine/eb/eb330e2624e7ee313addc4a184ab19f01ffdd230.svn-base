-- DOCUMENT
INSERT
    INTO
        INDEXDATA_MGT
            SELECT
                    INDEXDATA_MGT_ID_SEQ.nextval
                    ,CONTENTS.id
                    ,'TRANSACTION_OBJECT'
                    ,'DOCUMENT'
                    ,'DOCUMENT'
                    ,'CU'
                    ,sysdate
                FROM
                    (
                        SELECT
                                id
                            FROM
                                eimobj
                            WHERE
                                TYPE IN (
                                    SELECT
                                            id
                                        FROM
                                            eimobjtype
                                            START WITH (name = 'ドキュメント') CONNECT BY prior id = parent
                                )
                    ) CONTENTS;

-- FILE
INSERT
    INTO
        INDEXDATA_MGT
            SELECT
                    INDEXDATA_MGT_ID_SEQ.nextval
                    ,CONTENTS.id
                    ,'TRANSACTION_OBJECT'
                    ,'FILE'
                    ,'DOCUMENT'
                    ,'CU'
                    -- ドキュメントより先に処理されないように1秒加算する
                    ,sysdate + 1/86400
                FROM
                    (
                        SELECT
                                id
                            FROM
                                eimobj
                            WHERE
                                TYPE IN (
                                    SELECT
                                            id
                                        FROM
                                            eimobjtype
                                            START WITH (name = 'ドキュメント') CONNECT BY prior id = parent
                                )
                    ) CONTENTS;

-- FOLDER
INSERT
    INTO
        INDEXDATA_MGT
            SELECT
                    INDEXDATA_MGT_ID_SEQ.nextval
                    ,CONTENTS.id
                    ,'TRANSACTION_OBJECT'
                    ,'FOLDER'
                    ,'DOCUMENT'
                    ,'CU'
                    ,sysdate
                FROM
                    (
                        SELECT
                                id
                            FROM
                                eimobj
                            WHERE
                                TYPE IN (
                                    SELECT
                                            id
                                        FROM
                                            eimobjtype
                                            START WITH (name = 'フォルダ') CONNECT BY prior id = parent
                                )
                    ) CONTENTS;


-- TAG
INSERT
    INTO
        INDEXDATA_MGT
            SELECT
                    INDEXDATA_MGT_ID_SEQ.nextval
                    ,CONTENTS.id
                    ,'TRANSACTION_OBJECT'
                    ,'TAG'
                    ,'DOCUMENT'
                    ,'CU'
                    ,sysdate
                FROM
                    (
                        SELECT
                                id
                            FROM
                                eimobj
                            WHERE
                                TYPE IN (
                                    SELECT
                                            id
                                        FROM
                                            eimobjtype
                                            START WITH (name = 'タグ') CONNECT BY prior id = parent
                                )
                    ) CONTENTS;


-- ATTACHMENT
INSERT
    INTO
        INDEXDATA_MGT
            SELECT
                    INDEXDATA_MGT_ID_SEQ.nextval
                    ,CONTENTS.id
                    ,'TRANSACTION_OBJECT'
                    ,'ATTACHMENT'
                    ,'DOCUMENT'
                    ,'CU'
                    -- ドキュメントより先に処理されないように1秒加算する
                    ,sysdate + 1/86400
                FROM
                    (
                        SELECT
                                eimobj.id
                            FROM
                                eimobj
                                ,eimobjref
                            WHERE
                                eimobj.id = eimobjref.value
                                AND eimobj.TYPE IN (
                                    SELECT
                                            id
                                        FROM
                                            eimobjtype
                                        WHERE
                                            name = 'app.form.dev:帳票添付ファイル'
                                )
                                AND eimobjref.id IN (
                                    SELECT
                                            id
                                        FROM
                                            eimobj
                                        WHERE
                                            TYPE IN (
                                                SELECT
                                                        id
                                                    FROM
                                                        eimobjtype
                                                        START WITH (name = 'ドキュメント') CONNECT BY PRIOR id = parent
                                            )
                                )
                    ) CONTENTS;

