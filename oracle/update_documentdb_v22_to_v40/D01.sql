set serveroutput on size 100000
--①「公開通知」、「承認依頼」、「承認依頼通知」、「通知先」、「被承認依頼者」、「承認依頼者」、「承認者」、「再承認依頼先」、「差戻し」
--　のオブジェクトを削除した後、オブジェクトタイプを削除する。
--②「通知タイミング」、「ステータス」、「通知先種別：ID」、「被承認依頼者種別：ID」、「承認依頼者」、「承認者」、「承認日」、
--　「再承認依頼先種別：ID」、「差戻しユーザ」の属性値を削除した後、属性タイプを削除する。
--　※「承認依頼日」についてはEIMSTDATEから属性値を削除する。
--③①のオブジェクトタイプと②の属性タイプの関係を削除する
DECLARE
	
	--
	--オブジェクトタイプ
	--
	
	--公開通知
	objTypeId_PublicMail	number;
	--承認依頼
	objTypeId_Request		number;
	--承認依頼通知
	objTypeId_RequestMail	number;
	--通知先
	objTypeId_PublicTo		number;
	--被承認依頼者
	objTypeId_ApproveTo		number;
	--承認依頼者
	objTypeId_Requester		number;
	--承認者
	objTypeId_Approver		number;
	--再承認依頼先
	objTypeId_AgainRequestTo	number;
	--差戻し
	objTypeId_Return		number;
	
	--
	--属性タイプ
	--
	
	--通知タイミング
	attTypeId_Timing		number;
	--ステータス
	attTypeId_Status		number;
	--通知先種別：ID
	attTypeId_NoticeKindId	number;
	--被承認依頼者種別：ID
	attTypeId_ApproveKindId	number;
	--承認依頼者
	attTypeId_RequestUserId	number;
	--承認者
	attTypeId_ApproveUserId	number;
	--承認日
	attTypeId_ApproveDate	number;
	--再承認依頼先種別：ID
	attTypeId_AgainRequestKindId	number;
	--差戻しユーザ
	attTypeId_ReturnUserId	number;
	--承認依頼日
	attTypeId_RequestDate	number;
	
	rscode				number;
	
	
BEGIN
	
	--オブジェクトタイプIDと属性タイプIDを取得する
	
	--公開通知
	select id into objTypeId_PublicMail from EIMOBJTYPE where name = '公開通知';
	--承認依頼
	select id into objTypeId_Request from EIMOBJTYPE where name = '承認依頼';
	--承認依頼通知
	select id into objTypeId_RequestMail from EIMOBJTYPE where name = '承認依頼通知';
	--通知先
	select id into objTypeId_PublicTo from EIMOBJTYPE where name = '通知先';
	--被承認依頼者
	select id into objTypeId_ApproveTo from EIMOBJTYPE where name = '被承認依頼者';
	--承認依頼者
	select id into objTypeId_Requester from EIMOBJTYPE where name = '承認依頼者';
	--承認者
	select id into objTypeId_Approver from EIMOBJTYPE where name = '承認者';
	--再承認依頼先
	select id into objTypeId_AgainRequestTo from EIMOBJTYPE where name = '再承認依頼先';
	--差戻し
	select id into objTypeId_Return from EIMOBJTYPE where name = '差戻し';
	
	/*
	DBMS_OUTPUT.PUT_LINE('公開通知=' ||  );
	DBMS_OUTPUT.PUT_LINE('承認依頼=' ||  );
	DBMS_OUTPUT.PUT_LINE('承認依頼通知=' ||  );
	DBMS_OUTPUT.PUT_LINE('通知先=' ||  );
	DBMS_OUTPUT.PUT_LINE('被承認依頼者=' ||  );
	DBMS_OUTPUT.PUT_LINE('承認依頼者=' ||  );
	DBMS_OUTPUT.PUT_LINE('承認者=' ||  );
	DBMS_OUTPUT.PUT_LINE('再承認依頼先=' ||  );
	DBMS_OUTPUT.PUT_LINE('差戻し=' ||  );
	*/
	
	--通知タイミング
	select id into attTypeId_Timing from EIMATTR where name = '通知タイミング';
	--ステータス
	select id into attTypeId_Status from EIMATTR where name = 'ステータス';
	--通知先種別：ID
	select id into attTypeId_NoticeKindId from EIMATTR where name = '通知先種別：ID';
	--被承認依頼者種別：ID
	select id into attTypeId_ApproveKindId from EIMATTR where name = '被承認依頼者種別：ID';
	--承認依頼者
	select id into attTypeId_RequestUserId from EIMATTR where name = '承認依頼者';
	--承認者
	select id into attTypeId_ApproveUserId from EIMATTR where name = '承認者';
	--承認日
	select id into attTypeId_ApproveDate from EIMATTR where name = '承認日';
	--再承認依頼先種別：ID
	select id into attTypeId_AgainRequestKindId from EIMATTR where name = '再承認依頼先種別：ID';
	--差戻しユーザ
	select id into attTypeId_ReturnUserId from EIMATTR where name = '差戻しユーザ';
	--承認依頼日
	select id into attTypeId_RequestDate from EIMATTR where name = '承認依頼日';
	
	/*-----------------------------------------------------------------*/
	--属性値を削除する
	/*-----------------------------------------------------------------*/
	--通知タイミング
	delete from EIMOBJINT where type = attTypeId_Timing;
	--ステータス
	delete from EIMOBJINT where type = attTypeId_Status;
	--通知先種別：ID
	delete from EIMOBJSTR where type = attTypeId_NoticeKindId;
	--被承認依頼者種別：ID
	delete from EIMOBJSTR where type = attTypeId_ApproveKindId;
	--承認依頼者
	delete from EIMOBJINT where type = attTypeId_RequestUserId;
	--承認者
	delete from EIMOBJINT where type = attTypeId_ApproveUserId;
	--承認日
	delete from EIMOBJDATE where type = attTypeId_ApproveDate;
	--再承認依頼先種別：ID
	delete from EIMOBJSTR where type = attTypeId_AgainRequestKindId;
	--差戻しユーザ
	delete from EIMOBJINT where type = attTypeId_ReturnUserId;
	--承認依頼日（承認依頼日はステータス属性なのでEIMSTDATEから削除する）
	delete from EIMSTDATE where type = attTypeId_RequestDate;
	
	/*-----------------------------------------------------------------*/
	--オブジェクトを削除する
	/*-----------------------------------------------------------------*/
	--公開通知
	delete from EIMOBJ where type = objTypeId_PublicMail;
	--承認依頼
	delete from EIMOBJ where type = objTypeId_Request;
	--承認依頼通知
	delete from EIMOBJ where type = objTypeId_RequestMail;
	--通知先
	delete from EIMOBJ where type = objTypeId_PublicTo;
	--被承認依頼者
	delete from EIMOBJ where type = objTypeId_ApproveTo;
	--承認依頼者
	delete from EIMOBJ where type = objTypeId_Requester;
	--承認者
	delete from EIMOBJ where type = objTypeId_Approver;
	--再承認依頼先
	delete from EIMOBJ where type = objTypeId_AgainRequestTo;
	--差戻し
	delete from EIMOBJ where type = objTypeId_Return;
	
	/*-----------------------------------------------------------------*/
	--オブジェクトタイプと属性タイプの関係を削除する
	/*-----------------------------------------------------------------*/
	--公開通知
	delete from EIMATTRTYPE where type = objTypeId_PublicMail;
	--承認依頼
	delete from EIMATTRTYPE where type = objTypeId_Request;
	--承認依頼通知
	delete from EIMATTRTYPE where type = objTypeId_RequestMail;
	--通知先
	delete from EIMATTRTYPE where type = objTypeId_PublicTo;
	--被承認依頼者
	delete from EIMATTRTYPE where type = objTypeId_ApproveTo;
	--承認依頼者
	delete from EIMATTRTYPE where type = objTypeId_Requester;
	--承認者
	delete from EIMATTRTYPE where type = objTypeId_Approver;
	--再承認依頼先
	delete from EIMATTRTYPE where type = objTypeId_AgainRequestTo;
	--差戻し
	delete from EIMATTRTYPE where type = objTypeId_Return;
	
	/*-----------------------------------------------------------------*/
	--属性タイプ（OHTERを含む）を削除する
	/*-----------------------------------------------------------------*/
	--通知タイミング
	delete from EIMATTR where id = attTypeId_Timing;
	delete from EIMATTROTHER where aid = attTypeId_Timing;
	--ステータス
	delete from EIMATTR where id = attTypeId_Status;
	delete from EIMATTROTHER where aid = attTypeId_Status;
	--通知先種別：ID
	delete from EIMATTR where id = attTypeId_NoticeKindId;
	delete from EIMATTROTHER where aid = attTypeId_NoticeKindId;
	--被承認依頼者種別：ID
	delete from EIMATTR where id = attTypeId_ApproveKindId;
	delete from EIMATTROTHER where aid = attTypeId_ApproveKindId;
	--承認依頼者
	delete from EIMATTR where id = attTypeId_RequestUserId;
	delete from EIMATTROTHER where aid = attTypeId_RequestUserId;
	--承認者
	delete from EIMATTR where id = attTypeId_ApproveUserId;
	delete from EIMATTROTHER where aid = attTypeId_ApproveUserId;
	--承認日
	delete from EIMATTR where id = attTypeId_ApproveDate;
	delete from EIMATTROTHER where aid = attTypeId_ApproveDate;
	--再承認依頼先種別：ID
	delete from EIMATTR where id = attTypeId_AgainRequestKindId;
	delete from EIMATTROTHER where aid = attTypeId_AgainRequestKindId;
	--差戻しユーザ
	delete from EIMATTR where id = attTypeId_ReturnUserId;
	delete from EIMATTROTHER where aid = attTypeId_ReturnUserId;
	--承認依頼日
	delete from EIMATTR where id = attTypeId_RequestDate;
	delete from EIMATTROTHER where aid = attTypeId_RequestDate;
	
	/*-----------------------------------------------------------------*/
	--オブジェクトタイプ（OHTERを含む）を削除する
	/*-----------------------------------------------------------------*/
	--公開通知
	delete from EIMOBJTYPE where id = objTypeId_PublicMail;
	delete from EIMOBJTYPEOTHER where oid = objTypeId_PublicMail;
	--承認依頼
	delete from EIMOBJTYPE where id = objTypeId_Request;
	delete from EIMOBJTYPEOTHER where oid = objTypeId_Request;
	--承認依頼通知
	delete from EIMOBJTYPE where id = objTypeId_RequestMail;
	delete from EIMOBJTYPEOTHER where oid = objTypeId_RequestMail;
	--通知先
	delete from EIMOBJTYPE where id = objTypeId_PublicTo;
	delete from EIMOBJTYPEOTHER where oid = objTypeId_PublicTo;
	--被承認依頼者
	delete from EIMOBJTYPE where id = objTypeId_ApproveTo;
	delete from EIMOBJTYPEOTHER where oid = objTypeId_ApproveTo;
	--承認依頼者
	delete from EIMOBJTYPE where id = objTypeId_Requester;
	delete from EIMOBJTYPEOTHER where oid = objTypeId_Requester;
	--承認者
	delete from EIMOBJTYPE where id = objTypeId_Approver;
	delete from EIMOBJTYPEOTHER where oid = objTypeId_Approver;
	--再承認依頼先
	delete from EIMOBJTYPE where id = objTypeId_AgainRequestTo;
	delete from EIMOBJTYPEOTHER where oid = objTypeId_AgainRequestTo;
	--差戻し
	delete from EIMOBJTYPE where id = objTypeId_Return;
	delete from EIMOBJTYPEOTHER where oid = objTypeId_Return;
	
	commit;
	
END;
/
