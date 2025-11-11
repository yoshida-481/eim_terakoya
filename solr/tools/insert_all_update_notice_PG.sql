-- DOCUMENT
WITH recursive R(id) AS (
        SELECT
                id
            FROM
                EIMOBJTYPE EOT
            WHERE
                name = 'ドキュメント'
    UNION
    SELECT
            EOT.id
        FROM
            EIMOBJTYPE EOT
            ,R
        WHERE
            R.id = EOT.parent
)
INSERT
    INTO
        INDEXDATA_MGT
            SELECT
                    NEXTVAL('INDEXDATA_MGT_ID_SEQ')
                    ,CONTENTS.id
                    ,'TRANSACTION_OBJECT'
                    ,'DOCUMENT'
                    ,'DOCUMENT'
                    ,'CU'
                    ,CLOCK_TIMESTAMP()
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
                                            R
                                )
                    ) CONTENTS;

-- FILE
WITH recursive R(id) AS (
        SELECT
                id
            FROM
                EIMOBJTYPE EOT
            WHERE
                name = 'ドキュメント'
    UNION
    SELECT
            EOT.id
        FROM
            EIMOBJTYPE EOT
            ,R
        WHERE
            R.id = EOT.parent
)
INSERT
    INTO
        INDEXDATA_MGT
            SELECT
                    NEXTVAL('INDEXDATA_MGT_ID_SEQ')
                    ,CONTENTS.id
                    ,'TRANSACTION_OBJECT'
                    ,'FILE'
                    ,'DOCUMENT'
                    ,'CU'
                    -- ドキュメントより先に処理されないように1秒加算する
                    ,CLOCK_TIMESTAMP() + cast('1 seconds' as INTERVAL)
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
                                            R
                                )
                    ) CONTENTS;

-- FOLDER
WITH recursive R(id) AS (
        SELECT
                id
            FROM
                EIMOBJTYPE EOT
            WHERE
                name = 'フォルダ'
    UNION
    SELECT
            EOT.id
        FROM
            EIMOBJTYPE EOT
            ,R
        WHERE
            R.id = EOT.parent
)
INSERT
    INTO
        INDEXDATA_MGT
            SELECT
                    NEXTVAL('INDEXDATA_MGT_ID_SEQ')
                    ,CONTENTS.id
                    ,'TRANSACTION_OBJECT'
                    ,'FOLDER'
                    ,'DOCUMENT'
                    ,'CU'
                    ,CLOCK_TIMESTAMP()
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
                                            R
                                )
                    ) CONTENTS;


-- TAG
WITH recursive R(id) AS (
        SELECT
                id
            FROM
                EIMOBJTYPE EOT
            WHERE
                name = 'タグ'
    UNION
    SELECT
            EOT.id
        FROM
            EIMOBJTYPE EOT
            ,R
        WHERE
            R.id = EOT.parent
)
INSERT
    INTO
        INDEXDATA_MGT
            SELECT
                    NEXTVAL('INDEXDATA_MGT_ID_SEQ')
                    ,CONTENTS.id
                    ,'TRANSACTION_OBJECT'
                    ,'TAG'
                    ,'DOCUMENT'
                    ,'CU'
                    ,CLOCK_TIMESTAMP()
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
                                            R
                                )
                    ) CONTENTS;


-- ATTACHMENT
WITH recursive R(id) AS (
        SELECT
                id
            FROM
                EIMOBJTYPE EOT
            WHERE
                name = 'ドキュメント'
    UNION
    SELECT
            EOT.id
        FROM
            EIMOBJTYPE EOT
            ,R
        WHERE
            R.id = EOT.parent
)
INSERT
    INTO
        INDEXDATA_MGT
            SELECT
                    NEXTVAL('INDEXDATA_MGT_ID_SEQ')
                    ,CONTENTS.id
                    ,'TRANSACTION_OBJECT'
                    ,'ATTACHMENT'
                    ,'DOCUMENT'
                    ,'CU'
                    -- ドキュメントより先に処理されないように1秒加算する
                    ,CLOCK_TIMESTAMP() + cast('1 seconds' as INTERVAL)
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
                                                        R
                                            )
                                )
                    ) CONTENTS;

