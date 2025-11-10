set serveroutput on

declare

	type ref_cursor is ref cursor;

	--**********
	-- 定数
	--**********
	IS_NOT_MULTIPLE	constant	 number := 0;
	IS_MULTIPLE 	constant	 number := 1;

	CONS_OBJ_TYPE_NAME				constant varchar2(255)	:= 'ワークフロー設定';
	CONS_BASE_EVT_TYPE_REQ_APPROVE	constant number			:= -14001;				-- ベースイベントタイプ：承認依頼
	CONS_BASE_EVT_TYPE_APPROVAL		constant number			:= -14002;				-- ベースイベントタイプ：承認

	--**********
	-- 変数
	--**********
	rscode								number;
	attTypeName							varchar2(255);
	attTypeOtherName					varchar2(255);

	-- メール通知オブジェクトタイプID
	objTypeId_MailNotify				number;
	-- ドキュメントオブジェクトタイプID
	objTypeId_Document					number;
	-- フォルダオブジェクトタイプID
	objTypeId_Folder					number;

	-- 公開コメント属性ID
	attTypeId_PublicComment				number;
	-- 公開コメントログ属性ID
	attTypeId_PublicCommentLog			number;
	--- コメント属性ID
	attTypeId_Comment					number;
	--- 削除日時属性ID
	attTypeId_DeleteDate				number;

	eventTypeCursor						ref_cursor;
	eventType							EIMEVTYPE%rowtype;

	appException						exception;

	-- ドキュメント管理ワークフローID取得(帳票用WFの場合はワークフロー設定オブジェクトが存在しない)
	cursor c_document_wf is
	select obj.id as objid
			 , to_number(obj.name) as wfid
		from EIMOBJ obj
		where obj.type = (select wf_type.id
		                     from EIMOBJTYPE wf_type
		                    where wf_type.name = CONS_OBJ_TYPE_NAME)
	;

	duplex_check						number;

	/*------------------------------------------------------------------------
		[機能概要]
		属性文字列型⇒テキスト型変換処理
		・EIMEVTEXTへのinsert、EIMEVSTRへのdeleteを実行する
		・EIMSTTEXTへのinsert、EIMSTSTRへのdeleteを実行する
		・EIMOBJTEXTへのinsert、EIMOBJSTRへのdeleteを実行する
		・EIMRELTEXTへのinsert、EIMRELSTRへのdeleteを実行する
		・EIMATTRDEFTEXTへのinsert、EIMATTRDEFSTRへのdeleteを実行する
		・EIMATTRへのupdateを実行する
		・EIMEV***、EIMST***、EIMOBJ***、EIMREL***は一定件数ごとにコミットする
		・EIMATTR***は処理終了時にコミットする
		[引数]
		1. IN: I_attTypeId: 属性タイプID
	------------------------------------------------------------------------*/
	procedure updateAttTypeStrToText(
		I_attTypeId	in	number
	)
	is
		VALUE_TYPE_TEXT	constant number	:=	4;		-- テキスト型ValueType
		COMMIT_INTERVAL	constant number	:=	10000;	-- コミット間隔件数
		cnt				number;
	begin
		-- EIMEVSTR⇒EIMEVTEXT
		loop
			select count(*) into cnt from EIMEVSTR where type = I_attTypeId;

			exit when cnt = 0;

			insert into EIMEVTEXT (evid, type, key, value)
				select evid, type, key, to_clob(value)
					from EIMEVSTR
					where rowid in (
						select rowid from (
							select * from EIMEVSTR
								where type = I_attTypeId
								order by evid, key)
						where rownum <= COMMIT_INTERVAL);

			delete from EIMEVSTR
				where rowid in (
					select rowid from (
						select * from EIMEVSTR
							where type = I_attTypeId
							order by evid, key)
					where rownum <= COMMIT_INTERVAL);

			commit;
			--exit when 1 = 1;
		end loop;

		-- EIMSTSTR⇒EIMSTTEXT
		loop
			select count(*) into cnt from EIMSTSTR where type = I_attTypeId;

			exit when cnt = 0;

			insert into EIMSTTEXT (id, type, key, value)
				select id, type, key, to_clob(value)
					from EIMSTSTR
					where rowid in (
						select rowid from (
							select * from EIMSTSTR
								where type = I_attTypeId
								order by id, key)
						where rownum <= COMMIT_INTERVAL);

			delete from EIMSTSTR
				where rowid in (
					select rowid from (
						select * from EIMSTSTR
							where type = I_attTypeId
							order by id, key)
					where rownum <= COMMIT_INTERVAL);

			commit;
			--exit when 1 = 1;
		end loop;

		-- EIMOBJSTR⇒EIMOBJTEXT
		loop
			select count(*) into cnt from EIMOBJSTR where type = I_attTypeId;

			exit when cnt = 0;

			insert into EIMOBJTEXT (id, type, key, value)
				select id, type, key, to_clob(value)
					from EIMOBJSTR
					where rowid in (
						select rowid from (
							select * from EIMOBJSTR
								where type = I_attTypeId
								order by id, key)
						where rownum <= COMMIT_INTERVAL);

			delete from EIMOBJSTR
				where rowid in (
					select rowid from (
						select * from EIMOBJSTR
							where type = I_attTypeId
							order by id, key)
					where rownum <= COMMIT_INTERVAL);

			commit;
			--exit when 1 = 1;
		end loop;

		-- EIMRELSTR⇒EIMRELTEXT
		loop
			select count(*) into cnt from EIMRELSTR where type = I_attTypeId;

			exit when cnt = 0;

			insert into EIMRELTEXT (id, type, key, value)
				select id, type, key, to_clob(value)
					from EIMRELSTR
					where rowid in (
						select rowid from (
							select * from EIMRELSTR
								where type = I_attTypeId
								order by id, key)
						where rownum <= COMMIT_INTERVAL);

			delete from EIMRELSTR
				where rowid in (
					select rowid from (
						select * from EIMRELSTR
							where type = I_attTypeId
							order by id, key)
					where rownum <= COMMIT_INTERVAL);

			commit;
			--exit when 1 = 1;
		end loop;

		-- デフォルト値
		insert into EIMATTRDEFTEXT (id, key, value) select id, key, value from EIMATTRDEFSTR where id = I_attTypeId;
		delete from EIMATTRDEFSTR where id = I_attTypeId;

		-- 属性タイプ
		update EIMATTR set type = VALUE_TYPE_TEXT where id = I_attTypeId;

		commit;
		--rollback;

	end updateAttTypeStrToText;

begin

	/* ==================== */
	/* 属性タイプを作成する */
	/* ==================== */
	attTypeName := '公開通知コメント';
	attTypeOtherName := 'Public notification comment';
	AttributeUtils.createAttributeType(null, null, attTypeName, 4, IS_NOT_MULTIPLE, attTypeId_PublicComment, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicComment, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicComment, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;

	attTypeName := '公開通知コメントログ';
	attTypeOtherName := 'Public notification comment log';
	AttributeUtils.createAttributeType(null, null, attTypeName, 4, IS_NOT_MULTIPLE, attTypeId_PublicCommentLog, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicCommentLog, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicCommentLog, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;

	attTypeName := '削除日時';
	attTypeOtherName := 'Delete Date';
	AttributeUtils.createAttributeType(null, null, attTypeName, 3, IS_NOT_MULTIPLE, attTypeId_DeleteDate, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DeleteDate, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DeleteDate, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;


	/* ========================================== */
	/* オブジェクトタイプに属性タイプを割り当てる */
	/* ========================================== */
	select id into objTypeId_MailNotify from EIMOBJTYPE where name = 'メール通知';
	ObjectAttributeUtils.applyAttributeType(objTypeId_MailNotify, attTypeId_PublicComment, rscode);
	if rscode <> 0 then
		dbms_output.put_line('メール通知オブジェクトタイプに公開通知コメント属性タイプを付与できませんでした。');
		raise appException;
	end if;

	select id into objTypeId_Document from EIMOBJTYPE where name = 'ドキュメント';
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_DeleteDate, rscode);
	if rscode <> 0 then
		dbms_output.put_line('ドキュメントオブジェクトタイプに削除日時属性タイプを付与できませんでした。');
		raise appException;
	end if;

	select id into objTypeId_Folder from EIMOBJTYPE where name = 'フォルダ';
	ObjectAttributeUtils.applyAttributeType(objTypeId_Folder, attTypeId_DeleteDate, rscode);
	if rscode <> 0 then
		dbms_output.put_line('フォルダオブジェクトタイプに削除日時属性タイプを付与できませんでした。');
		raise appException;
	end if;

	/* ========================================================== */
	/* 既存のイベントタイプに公開コメントログ属性タイプを付与する */
	/* ========================================================== */
	for r_document_wf in c_document_wf loop
		-- イベントタイプに公開通知コメントログ属性タイプ付与
		-- ベースイベントタイプが承認・承認依頼の場合のみ
		EventTypeDao2.getListByCriteria(r_document_wf.wfid, null, null, null, 'JA', null, null, null, null, eventTypeCursor, rscode);
		if rscode <> 0 then
			dbms_output.put_line('ワークフロー「' || r_document_wf.wfid || '」のイベントタイプを取得できませんでした。');
			raise appException;
		end if;

		loop
			fetch eventTypeCursor into eventType;
			exit when eventTypeCursor%notfound;
			if eventType.bevtid = CONS_BASE_EVT_TYPE_REQ_APPROVE or eventType.bevtid = CONS_BASE_EVT_TYPE_APPROVAL then
				-- 重複登録チェック
				select count(*) into duplex_check
					from eimattrtype at
					where at.type = eventType.evtid and at.id = attTypeId_PublicCommentLog;
				if duplex_check = 0 then
					-- 重複していない場合のみ属性タイプを付与する
					EventTypeDao2.addAttributeType(eventType.evtid, attTypeId_PublicCommentLog, null, rscode);
					if rscode <> 0 then
						dbms_output.put_line('「公開通知コメントログ」属性タイプをイベントタイプ「' || eventType.evtid || '」に設定できませんでした。');
						raise appException;
					end if;
					dbms_output.put_line('「公開通知コメントログ」属性タイプをイベントタイプに設定しました。evtid：'|| eventType.evtid || 'evtname：' || eventType.name);
				end if;
			end if;
		end loop;
		close eventTypeCursor;
	end loop;

	commit;
	--rollback;

	/* ============================================== */
	/* コメント属性を文字列型からテキスト型に変更する */
	/* ============================================== */
	attTypeName := 'コメント';
	select id into attTypeId_Comment from EIMATTR where name = attTypeName;
	updateAttTypeStrToText(attTypeId_Comment);
	dbms_output.put_line('「' || attTypeName || '」属性タイプを文字列型からテキスト型に変更しました。id：' || attTypeId_Comment);

exception
	when appException then
		dbms_output.put_line('rscode=[ ' || rscode || ' ]');
		rollback;
	when OTHERS then
		dbms_output.put_line('エラー発生: ' || sqlerrm(sqlcode));
		rollback;

end;
/
