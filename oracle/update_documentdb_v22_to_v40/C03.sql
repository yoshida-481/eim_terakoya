set serveroutput on size 100000
DECLARE
	
	--**********
	--定数
	--**********
	--テーブルレコード処理用の添え字
	----------------------------------------------
	--ステータスタイプ種別
	----------------------------------------------
	--編集中
	CONS_STT_EDIT		constant number := -13001;
	--承認依頼中
	CONS_STT_APPREQ		constant number := -13002;
	--公開処理中
	CONS_STT_PUBLICING	constant number := -13003;
	--公開済み
	CONS_STT_PUBLIC		constant number := -13004;
	
	----------------------------------------------
	--ベースイベントタイプ
	----------------------------------------------
	--承認依頼
	CONS_BET_APPREQ		constant number := -14001;
	CONS_BET_APPREQ_JA_NAME	constant varchar2(256) := '承認依頼';
	CONS_BET_APPREQ_EN_NAME	constant varchar2(256) := 'Approval request';
	--承認
	CONS_BET_APPROVE	constant number := -14002;
	CONS_BET_APPROVE_JA_NAME	constant varchar2(256) := '承認';
	CONS_BET_APPROVE_EN_NAME	constant varchar2(256) := 'Approval';
	--公開
	CONS_BET_PUBLIC		constant number := -14003;
	CONS_BET_PUBLIC_JA_NAME	constant varchar2(256) := '公開';
	CONS_BET_PUBLIC_EN_NAME	constant varchar2(256) := 'Public';

	--承認依頼取消
	CONS_BET_CANAPPREQ		constant number := -14004;
	CONS_BET_CANAPPREQ_JA_NAME	constant varchar2(256) := '承認依頼取消';
	CONS_BET_CANAPPREQ_EN_NAME	constant varchar2(256) := 'Cancel approval request';

	--差戻し
	CONS_BET_SENDBACK		constant number := -14005;
	CONS_BET_SENDBACK_JA_NAME	constant varchar2(256) := '差戻し';
	CONS_BET_SENDBACK_EN_NAME	constant varchar2(256) := 'Send back';
	----------------------------------------------
	--ガード条件
	----------------------------------------------
	CONS_GUD_MINUS_15001	constant number := -15001;
	CONS_GUD_MINUS_15002	constant number := -15002;
	CONS_GUD_MINUS_15003	constant number := -15003;
	CONS_GUD_MINUS_15004	constant number := -15004;
	CONS_GUD_MINUS_15005	constant number := -15005;
	CONS_GUD_MINUS_15006	constant number := -15006;
	----------------------------------------------
	--メール種別
	----------------------------------------------
	CONS_MAIL_MINUS_12001	constant number := -12001;
	CONS_MAIL_MINUS_12002	constant number := -12002;
	CONS_MAIL_MINUS_12003	constant number := -12003;
	CONS_MAIL_MINUS_12004	constant number := -12004;
	CONS_MAIL_MINUS_12005	constant number := -12005;

	--**********
	--変数
	--**********
	--テーブルレコード処理用の添え字
	idx	number := 0;
	rcode	number := 0;
	tmp_eventTypeId number := null;
	eimaceIdx number := 0;
	
	--**********
	--カーソル
	--**********
	--ワークフロー
	cursor c_eimwf is select * from eimwf order by id;
	--ステータスタイプ
	cursor c_eimsttype(p_workflowId number) is
		select
			 EST.id
			,EST.name
			,EST.workflow
			,EST.kind
			,EST.auto
		from EIMSTTYPE EST, EIMSTTYPEREL ESTR
		where
			EST.id = ESTR.child
		start with child = (
			select id from EIMSTTYPE EST, EIMSTTYPEREL ESTR
			where EST.workflow = p_workflowId
				and EST.id = ESTR.child
				and ESTR.parent = 0
		)
		connect by prior ESTR.child = ESTR.parent
		order by level;
	
	--承認者エントリー
	cursor c_eimace(p_statusTypeId number) is
		select
			 eid
			,sid
			,type
			,entry
			,priority
		from EIMACE
		where
			sid = p_statusTypeId
		order by priority,eid
	;
	
	--**********
	--配列
	--**********
	--EIMSTTYPE
	type type_tab_eimsttype is table of eimsttype%rowtype index by binary_integer;
	--１つのワークフローに紐づくステータスタイプ(グローバル扱い)
	tab_eimsttype type_tab_eimsttype;
	
	--EIMACE
	type type_tab_eimace is table of eimace%rowtype index by binary_integer;
	tab_eimace type_tab_eimace;
	
	--**********
	--プロシージャ/ファンクション
	--**********
	
	--
	--時刻を表示
	--
	PROCEDURE dsp_Time
	IS
		timeStr	varchar2(1000) := '';
	BEGIN
		select to_char(sysdate,'YYYY/MM/DD HH24:MI:SS') into timeStr from dual;
		DBMS_OUTPUT.PUT_LINE(timeStr);
	END;

	--
	--ステータスタイプIDでEIMSTTYPEのkind列とauto列を更新する
	--
	PROCEDURE upd_kindAndAuto( p_sttrec IN eimsttype%rowtype)
	IS
	BEGIN
		--DBMS_OUTPUT.PUT_LINE(p_sttrec.kind);
		IF p_sttrec.kind = 1 THEN
			update EIMSTTYPE set kind = CONS_STT_EDIT, auto = 1 where id = p_sttrec.id;
		ELSIF p_sttrec.kind = 2 OR  p_sttrec.kind = 3 THEN
			update EIMSTTYPE set kind = CONS_STT_APPREQ, auto = 0 where id = p_sttrec.id;
		ELSIF p_sttrec.kind = 4 THEN
			update EIMSTTYPE set kind = CONS_STT_PUBLICING, auto = 1 where id = p_sttrec.id;
		ELSIF p_sttrec.kind = 5 THEN
			update EIMSTTYPE set kind = CONS_STT_PUBLIC, auto = 1 where id = p_sttrec.id;
		END IF;
	END;
	
	--
	--ベースイベントタイプとガード条件でEIMNOTICEMAILを作成する
	--
	PROCEDURE ins_EIMNOTICEMAIL
	(
		 p_baseEventTypeId IN number
		,p_guardCondId IN number
		,p_eventTypeId IN number
	)
	IS
	BEGIN
		--DBMS_OUTPUT.PUT_LINE(p_sttrec.kind);
		IF p_baseEventTypeId = CONS_BET_APPREQ THEN
			insert into EIMNOTICEMAIL values(EIMID.nextval,		p_eventTypeId,	CONS_MAIL_MINUS_12001,	2);
		ELSIF p_baseEventTypeId = CONS_BET_APPROVE AND p_guardCondId = CONS_GUD_MINUS_15001 THEN
			insert into EIMNOTICEMAIL values(EIMID.nextval,		p_eventTypeId,	CONS_MAIL_MINUS_12001,	2);
		ELSIF p_baseEventTypeId = CONS_BET_APPROVE AND p_guardCondId = CONS_GUD_MINUS_15003 THEN
			insert into EIMNOTICEMAIL values(EIMID.nextval,		p_eventTypeId,	CONS_MAIL_MINUS_12002,	0);
		ELSIF p_baseEventTypeId = CONS_BET_APPROVE AND p_guardCondId = CONS_GUD_MINUS_15004 THEN
			insert into EIMNOTICEMAIL values(EIMID.nextval,		p_eventTypeId,	CONS_MAIL_MINUS_12002,	0);
			insert into EIMNOTICEMAIL values(EIMID.nextval,		p_eventTypeId,	CONS_MAIL_MINUS_12005,	2);
		ELSIF p_baseEventTypeId = CONS_BET_CANAPPREQ AND p_guardCondId = CONS_GUD_MINUS_15005 THEN
			insert into EIMNOTICEMAIL values(EIMID.nextval,		p_eventTypeId,	CONS_MAIL_MINUS_12004,	2);
		ELSIF p_baseEventTypeId = CONS_BET_CANAPPREQ AND p_guardCondId = CONS_GUD_MINUS_15006 THEN
			insert into EIMNOTICEMAIL values(EIMID.nextval,		p_eventTypeId,	CONS_MAIL_MINUS_12004,	2);
		ELSIF p_baseEventTypeId = CONS_BET_SENDBACK THEN
			insert into EIMNOTICEMAIL values(EIMID.nextval,		p_eventTypeId,	CONS_MAIL_MINUS_12003,	2);
		ELSIF p_baseEventTypeId = CONS_BET_PUBLIC THEN
			insert into EIMNOTICEMAIL values(EIMID.nextval,		p_eventTypeId,	CONS_MAIL_MINUS_12005,	2);
		END IF;
	END;
	
	
	--
	--ドキュメント管理使用できるワークフローかどうかをチェックする
	--ワークフローがドキュメント管理用の場合はtrueを返却
	--
	FUNCTION bln_checkDocWorkflow
	RETURN number
	IS
		cnt				number := 0;
		findFlg			boolean := false;
		findCntEdit		number := 0;
		findCntPublicing	number := 0;
		findCntPublic		number := 0;
	BEGIN
		cnt := tab_eimsttype.count;
		--ステータスタイプが４つ以上ない
		IF cnt < 4 THEN
			return -1;
		END IF;
		
		--編集中ステータスタイプが１番目でない
		IF tab_eimsttype(0).kind <> 1 THEN
			return -2;
		END IF;
		
		--承認依頼中(KIND=2or3)ステータスタイプがない
		FOR i IN 0..cnt-1 LOOP
			IF tab_eimsttype(i).kind = 2 OR tab_eimsttype(i).kind = 3 THEN
				findFlg := true;
				EXIT;
			END IF;
		END LOOP;
		IF findFlg = false THEN
			return -3;
		END IF;
		
		--編集中・公開処理中・公開済みステータスタイプが２つ以上存在する
		FOR i IN 0..cnt-1 LOOP
			IF tab_eimsttype(i).kind = 1 THEN
				findCntEdit := findCntEdit + 1;
			END IF;
			IF tab_eimsttype(i).kind = 4 THEN
				findCntPublicing := findCntPublicing + 1;
			END IF;
			IF tab_eimsttype(i).kind = 5 THEN
				findCntPublic := findCntPublic + 1;
			END IF;
		END LOOP;
		IF findCntEdit >= 2 OR findCntPublicing >= 2 OR findCntPublic >= 2 THEN
			return -4;
		END IF;
		
		--一番後ろのステータスタイプが公開済みでない
		IF tab_eimsttype(tab_eimsttype.count-1).kind <> 5 THEN
			return -5;
		END IF;
		--後ろから２番目のステータスタイプが公開処理中でない
		IF tab_eimsttype(tab_eimsttype.count-2).kind <> 4 THEN
			return -6;
		END IF;
		
		return 0;
	END;
	
BEGIN
	
	FOR r_eimwf IN c_eimwf LOOP
		
		insert into IKOU_TARGET_WORKFLOW(workflow,ikou_flg) values(r_eimwf.id,0);
		
		--EIMSTTYPEの値を配列に全て格納する
		FOR r_eimsttype IN c_eimsttype(r_eimwf.id) LOOP
			tab_eimsttype(idx) := r_eimsttype;
			idx := idx + 1;
		END LOOP;
		
		IF tab_eimsttype.count = 0 THEN
			update IKOU_TARGET_WORKFLOW set ikou_flg = -9 where workflow = r_eimwf.id;
		END IF;
		
		--DBMS_OUTPUT.PUT_LINE(tab_eimsttype.count);
		FOR i IN 0..tab_eimsttype.count-1  LOOP
			
			--ドキュメント管理用に作成されたワークフローかどうかをチェックする
			rcode := 0;
			rcode := bln_checkDocWorkflow();
			IF rcode <> 0 THEN
				--DBMS_OUTPUT.PUT_LINE('移行できないワークフロー:' || tab_eimsttype(0).workflow || ':' || rcode);
				update IKOU_TARGET_WORKFLOW set ikou_flg = rcode where workflow = tab_eimsttype(i).workflow;
				EXIT;
			END IF;
			
			--EIMSTTYPEテーブルを更新
			upd_kindAndAuto(tab_eimsttype(i));
			
			--EIMEVTYPEとEIMEVTYPEOHTERを作成
			IF tab_eimsttype(i).kind = 1 THEN
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_APPREQ_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i+1).id,	CONS_BET_APPREQ,	CONS_GUD_MINUS_15001,	1);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_APPREQ, CONS_GUD_MINUS_15001, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_APPREQ_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_APPREQ_JA_NAME);
			ELSIF tab_eimsttype(i).kind = 2 AND (tab_eimsttype(i+1).kind = 2 OR tab_eimsttype(i+1).kind = 3) THEN
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_SENDBACK_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(0).id,	CONS_BET_SENDBACK,	CONS_GUD_MINUS_15001,	1);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_SENDBACK, CONS_GUD_MINUS_15001, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_SENDBACK_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_SENDBACK_JA_NAME);
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_CANAPPREQ_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i-1).id,	CONS_BET_CANAPPREQ,	CONS_GUD_MINUS_15006,	2);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_CANAPPREQ, CONS_GUD_MINUS_15006, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_CANAPPREQ_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_CANAPPREQ_JA_NAME);
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_CANAPPREQ_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i).id,	CONS_BET_CANAPPREQ,	CONS_GUD_MINUS_15005,	1);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_CANAPPREQ, CONS_GUD_MINUS_15005, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_CANAPPREQ_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_CANAPPREQ_JA_NAME);
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_APPROVE_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i).id,	CONS_BET_APPROVE,	CONS_GUD_MINUS_15002,	1);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_APPROVE, CONS_GUD_MINUS_15002, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_APPROVE_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_APPROVE_JA_NAME);
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_APPROVE_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i+1).id,	CONS_BET_APPROVE,	CONS_GUD_MINUS_15001,	2);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_APPROVE, CONS_GUD_MINUS_15001, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_APPROVE_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_APPROVE_JA_NAME);
			ELSIF tab_eimsttype(i).kind = 2 AND tab_eimsttype(i+1).kind = 4 THEN
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_SENDBACK_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(0).id,	CONS_BET_SENDBACK,	CONS_GUD_MINUS_15001,	1);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_SENDBACK, CONS_GUD_MINUS_15001, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_SENDBACK_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_SENDBACK_JA_NAME);
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_CANAPPREQ_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i-1).id,	CONS_BET_CANAPPREQ,	CONS_GUD_MINUS_15006,	2);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_CANAPPREQ, CONS_GUD_MINUS_15006, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_CANAPPREQ_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_CANAPPREQ_JA_NAME);
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_CANAPPREQ_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i).id,	CONS_BET_CANAPPREQ,	CONS_GUD_MINUS_15005,	1);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_CANAPPREQ, CONS_GUD_MINUS_15005, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_CANAPPREQ_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_CANAPPREQ_JA_NAME);
				
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_APPROVE_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i).id,	CONS_BET_APPROVE,	CONS_GUD_MINUS_15002,	1);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_APPROVE, CONS_GUD_MINUS_15002, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_APPROVE_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_APPROVE_JA_NAME);
				
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_APPROVE_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i+1).id,	CONS_BET_APPROVE,	CONS_GUD_MINUS_15003,	2);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_APPROVE, CONS_GUD_MINUS_15003, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_APPROVE_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_APPROVE_JA_NAME);
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_APPROVE_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i+2).id,	CONS_BET_APPROVE,	CONS_GUD_MINUS_15004,	3);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_APPROVE, CONS_GUD_MINUS_15004, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_APPROVE_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_APPROVE_JA_NAME);
			ELSIF tab_eimsttype(i).kind = 3 AND (tab_eimsttype(i+1).kind = 2 OR tab_eimsttype(i+1).kind = 3) THEN
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_SENDBACK_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(0).id,	CONS_BET_SENDBACK,	CONS_GUD_MINUS_15001,	1);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_SENDBACK, CONS_GUD_MINUS_15001, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_SENDBACK_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_SENDBACK_JA_NAME);
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_CANAPPREQ_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i-1).id,	CONS_BET_CANAPPREQ,	CONS_GUD_MINUS_15006,	1);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_CANAPPREQ, CONS_GUD_MINUS_15006, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_CANAPPREQ_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_CANAPPREQ_JA_NAME);
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_APPROVE_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i+1).id,	CONS_BET_APPROVE,	CONS_GUD_MINUS_15001,	1);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_APPROVE, CONS_GUD_MINUS_15001, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_APPROVE_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_APPROVE_JA_NAME);
			ELSIF tab_eimsttype(i).kind = 3 AND tab_eimsttype(i+1).kind = 4 THEN
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_SENDBACK_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(0).id,	CONS_BET_SENDBACK,	CONS_GUD_MINUS_15001,	1);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_SENDBACK, CONS_GUD_MINUS_15001, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_SENDBACK_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_SENDBACK_JA_NAME);
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_CANAPPREQ_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i-1).id,	CONS_BET_CANAPPREQ,	CONS_GUD_MINUS_15006,	1);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_CANAPPREQ, CONS_GUD_MINUS_15006, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_CANAPPREQ_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_CANAPPREQ_JA_NAME);
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_APPROVE_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i+1).id,	CONS_BET_APPROVE,	CONS_GUD_MINUS_15003,	1);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_APPROVE, CONS_GUD_MINUS_15003, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_APPROVE_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_APPROVE_JA_NAME);
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_APPROVE_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i+2).id,	CONS_BET_APPROVE,	CONS_GUD_MINUS_15004,	2);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_APPROVE, CONS_GUD_MINUS_15004, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_APPROVE_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_APPROVE_JA_NAME);
			ELSIF tab_eimsttype(i).kind = 4 THEN
				insert into EIMEVTYPE values(EIMID.nextval,	CONS_BET_PUBLIC_JA_NAME,	tab_eimsttype(i).id,	tab_eimsttype(i+1).id,	CONS_BET_PUBLIC,	CONS_GUD_MINUS_15001,	1);
				select EIMID.currval into tmp_eventTypeId from dual;
				ins_EIMNOTICEMAIL(CONS_BET_PUBLIC, CONS_GUD_MINUS_15001, tmp_eventTypeId);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'EN',	CONS_BET_PUBLIC_EN_NAME);
					insert into EIMEVTYPEOTHER values(tmp_eventTypeId,	'JA',	CONS_BET_PUBLIC_JA_NAME);
			END IF;
			
			--承認者エントリー(EIMACE)からアサイン先エントリーを作成する
			IF tab_eimsttype(i).kind = 2 OR tab_eimsttype(i).kind = 3 THEN
				
				eimaceIdx := 0;
				--EIMACE配列に格納する
				FOR r_eimace IN c_eimace(tab_eimsttype(i).id) LOOP
					tab_eimace(eimaceIdx) := r_eimace;
					eimaceIdx := eimaceIdx + 1;
				END LOOP;
				
				FOR II IN 0..tab_eimace.count-1 LOOP
					insert into EIMASENTRY values(EIMID.nextval, tab_eimace(II).sid, tab_eimace(II).type, tab_eimace(II).entry);
					delete from EIMACE where eid = tab_eimace(II).eid;
					delete from EIMACR where id = tab_eimace(II).eid;
				END LOOP;
				delete from EIMACR where id = tab_eimsttype(i).id;
				delete from EIMACU where sid = tab_eimsttype(i).id;
			END IF;
			
		END LOOP;
		
		IF rcode = 0 THEN
			update IKOU_TARGET_WORKFLOW set ikou_flg = 1 where workflow = r_eimwf.id;
		END IF;
		
		--EIMSTTYPEの配列をクリアする
		tab_eimsttype.delete;
		idx := 0;
	
		--DEBUG
		--EXIT;
		commit;
	
	END LOOP;

END;
/