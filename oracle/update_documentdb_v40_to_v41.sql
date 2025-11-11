set serveroutput on size 100000

declare

	--**********
	-- 定数
	--**********
	-- 言語ID
	CONS_LANG_CODE_JA			constant varchar2(2)	:= 'JA';
	CONS_LANG_CODE_EN			constant varchar2(2)	:= 'EN';

	-- イベント:「取戻し」登録用
	CONS_OBJ_TYPE_NAME		constant varchar2(512)	:= 'ワークフロー設定';	-- EIMOBJTYPE.name：「ワークフロー設定」
	CONS_STATUS_KIND_FROM	constant number			:= -13003;				-- EIMSTTYPE.kind：「公開処理中」の種別ID
	CONS_STATUS_KIND_TO		constant number			:= -13001;				-- EIMSTTYPE.kind：「編集中」の種別ID
	CONS_EVENT_NAME_JA		constant varchar2(512)	:= '取戻し';			-- 登録値：name(日本語)
	CONS_EVENT_NAME_EN		constant varchar2(512)	:= 'Take back';			-- 登録値：name(英語)
	CONS_BEVT_ID			constant number			:= -14006;				-- 登録値：bevtid(「取戻し」のコード値)
	CONS_GUARD_ID			constant number			:= -15001;				-- 登録値：guardid(「なし」のコード値)
	CONS_SEQ				constant number			:= 1;					-- 登録値：seq

	-- 属性:「ワークフロー設定上長承認設定」登録用
	CONS_IS_NOT_MULTIPLE		constant number := 0;
	CONS_ATTR_TYPE_NAME			constant varchar2(512)	:= 'ワークフロー設定上長承認設定';
	CONS_ATTR_TYPE_OTHER_NAME	constant varchar2(512)	:= 'Workflow setting boss approval';
	CONS_WORKFLOW_OBJ_STR_VALUE	constant varchar2(512)	:= 'necessary';
	CONS_WORKFLOW_OBJ_STR_KEY	constant number			:= 0;

	--**********
	-- 変数
	--**********
	-- イベント:「取戻し」登録用
	sttId_f			number;		-- 「取戻し」イベント登録用_FromID
	sttId_t			number;		-- 「取戻し」イベント登録用_ToID
	duplex_check	number;		-- 登録対象ID重複チェック

	-- 属性:「ワークフロー設定上長承認設定」登録用
	rscode							number;			-- リターンコード格納用
	objTypeId_WorkflowSetting		number;			-- オブジェクトタイプID格納用
	attTypeId_BossApprovalFlag		number;			-- 属性タイプID格納用
	attTypeName						varchar2(255);	-- 属性タイプ名格納用
	attTypeOtherName				varchar2(255);	-- 属性タイプ別名格納用

	--**********
	-- カーソル
	--**********
	-- ドキュメント管理ワークフローID取得
	cursor c_document_wf is
		select obj.id as objid
			 , to_number(obj.name) as wfid
		  from EIMOBJ obj
		 where obj.type = (select wf_type.id
		                     from EIMOBJTYPE wf_type
		                    where wf_type.name = CONS_OBJ_TYPE_NAME)
	;

	--************************************************************************
	-- プロシジャ
	--************************************************************************
	/*------------------------------------------------------------------------
		[機能概要]
		「取戻し」イベント登録用情報取得
		・EIMEVTYPEへ登録するSTTID_F, STTID_Tを取得する
		・「取戻し」が登録済みであるかをチェックする
		[引数]
		1. IN:  I_workflowId: 登録対象ワークフローID
		2. OUT: O_sttId_f: EIMEVTYPEへ登録するSTTID_F
		3. OUT: O_sttId_t: EIMEVTYPEへ登録するSTTID_T
		4. OUT: O_duplexExist: EIMEVTYPEへ登録対象のSTTID_F,STTID_Tで登録
		                       されている「取戻し」イベントの件数
	------------------------------------------------------------------------*/
	procedure getRegistInfo(
		I_workflowId	in	number
		, O_sttId_f		out	number
		, O_sttId_t		out	number
		, O_duplexExist	out	number
	)
	is

	begin

			-- 「取戻し」イベント登録用_FromID取得
		select status.id
		  into O_sttId_f
		  from EIMSTTYPE status
		 where status.kind = CONS_STATUS_KIND_FROM
		   and status.workflow = I_workflowId;

		-- 「取戻し」イベント登録用_ToID取得
		select status.id
		  into O_sttId_t
		  from EIMSTTYPE status
		 where status.kind = CONS_STATUS_KIND_TO
		   and status.workflow = I_workflowId;

		-- 登録対象ID重複チェック
		select count(ev.evtid)
		  into O_duplexExist
		  from EIMEVTYPE ev
		 where ev.name = CONS_EVENT_NAME_JA
		   and ev.sttid_f = O_sttId_f
		   and ev.sttid_t = O_sttId_t;

	end getRegistInfo;

	/*------------------------------------------------------------------------
		[機能概要]
		「取戻し」イベント登録処理
		・EIMEVTYPE, EIMEVTYPEOTHERへのinsertを実行する
		[引数]
		1. IN:  I_sttId_f: EIMEVTYPEへ登録するSTTID_F
		2. IN:  I_sttId_t: EIMEVTYPEへ登録するSTTID_T
	------------------------------------------------------------------------*/
	procedure insertEvent(
		  I_sttId_f		in	number
		, I_sttId_t		in	number
	)
	is
		eventId		number;
	begin
		-- PK(EVTID)を採番
		eventId := EIMID.nextval;

		-- EIMEVTYPE(イベントタイプ情報)へ登録
		insert into EIMEVTYPE
		       values(
		          eventId
		        , CONS_EVENT_NAME_JA
		        , I_sttId_f
		        , I_sttId_t
		        , CONS_BEVT_ID
		        , CONS_GUARD_ID
		        , CONS_SEQ
		       );

		-- EIMEVTYPEOTHER(他言語イベントタイプ情報)_日本語へ登録
		insert into EIMEVTYPEOTHER
		       values(
		          eventId
		        , CONS_LANG_CODE_JA
		        , CONS_EVENT_NAME_JA
		       );

		-- EIMEVTYPEOTHER(他言語イベントタイプ情報)_英語へ登録
		insert into EIMEVTYPEOTHER
		       values(
		          eventId
		        , CONS_LANG_CODE_EN
		        , CONS_EVENT_NAME_EN
		       );

	end insertEvent;

begin

	/* ================================================================== */
	-- 属性：「ワークフロー設定上長承認設定」追加
	/* ================================================================== */
	-- 「ワークフロー設定」のobjectTypeId取得
	select type.id
	  into objTypeId_WorkflowSetting
	  from EIMOBJTYPE type
	 where type.name = CONS_OBJ_TYPE_NAME;

	-- 属性登録
	attTypeName := CONS_ATTR_TYPE_NAME;
	attTypeOtherName := CONS_ATTR_TYPE_OTHER_NAME;
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, CONS_IS_NOT_MULTIPLE, attTypeId_BossApprovalFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_BossApprovalFlag, CONS_LANG_CODE_JA, attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_BossApprovalFlag, CONS_LANG_CODE_EN, attTypeOtherName, rscode);

	-- ワークフロー設定オブジェクトの属性リレーション登録
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowSetting, attTypeId_BossApprovalFlag, rscode);

	/* ================================================================== */
	-- ドキュメント管理のワークフローに対してイベント「取戻し」を登録する
	/* ================================================================== */
	for r_document_wf in c_document_wf loop

		-- 変数初期化
		sttId_f := null;
		sttId_t := null;
		duplex_check := 0;

		-- EIMEVTYPEへの登録ID取得、及び、重複チェック
		getRegistInfo(
			r_document_wf.wfid
		  , sttId_f
		  , sttId_t
		  , duplex_check
		);

		if ((sttId_f is not null) and (sttId_t is not null) and (duplex_check = 0)) then

			-- EIMEVTYPEへの登録
			insertEvent(
				sttId_f
			  , sttId_t
			);

		end if;

		-- EIMOBJSTR(属性情報：ワークフロー設定上長承認設定)を登録
		insert into EIMOBJSTR
		       values(
		          r_document_wf.objid
		        , attTypeId_BossApprovalFlag
		        , CONS_WORKFLOW_OBJ_STR_KEY
		        , CONS_WORKFLOW_OBJ_STR_VALUE
		       );

	end loop;
	
	commit;

exception
	when OTHERS then
		dbms_output.put_line('エラー発生: ' || sqlerrm(sqlcode));

end;
/
