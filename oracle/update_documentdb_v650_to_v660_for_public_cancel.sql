set serveroutput on 

declare

	--**********
	-- 定数
	--**********
	-- 言語ID
	CONS_LANG_CODE_JA			constant varchar2(2)	:= 'JA';
	CONS_LANG_CODE_EN			constant varchar2(2)	:= 'EN';
	
	-- イベント:「公開取消」登録用
	CONS_OBJ_TYPE_NAME		constant varchar2(512)	:= 'ワークフロー設定';	-- EIMOBJTYPE.name：「ワークフロー設定」
	CONS_STATUS_KIND_FROM	constant number			:= -13004;				-- EIMSTTYPE.kind：「公開済」の種別ID
	CONS_STATUS_KIND_TO		constant number			:= -13001;				-- EIMSTTYPE.kind：「編集中」の種別IID
	CONS_EVENT_NAME_JA		constant varchar2(512)	:= '公開取消';			-- 登録値：name(日本語)
	CONS_EVENT_NAME_EN		constant varchar2(512)	:= 'Public cancel';		-- 登録値：name(英語)
	CONS_BEVT_ID			constant number			:= -14007;				-- 登録値：bevtid(「公開取消」のコード値)
	CONS_GUARD_ID			constant number			:= -15001;				-- 登録値：guardid(「なし」のコード値)
	CONS_SEQ				constant number			:= 1;					-- 登録値：seq
	CONS_MAIL_TYPE			constant number			:= -12006;				-- メールタイプ(公開取消通知)
	CONS_MAIL_METHOD		constant number			:= 0;					-- 通知方法(即時)

	
	--**********
	-- 変数
	--**********
	-- イベント:「公開取消」登録用
	sttId_f			number;		-- 「公開取消」イベント登録用_FromID
	sttId_t			number;		-- 「公開取消」イベント登録用_ToID
	duplex_check	number;		-- 登録対象ID重複チェック
	wf_name			varchar2(512);		--「WF名称」
	
	--**********
	-- カーソル
	--**********
	-- ドキュメント管理ワークフローID取得(帳票用WFの場合はワークフロー設定オブジェクトが存在しない)
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
		「公開取消」イベント登録用情報取得
		・EIMEVTYPEへ登録するSTTID_F, STTID_Tを取得する
		・「公開取消」が登録済みであるかをチェックする
		[引数]
		1. IN:  I_workflowId: 登録対象ワークフローID
		2. OUT: O_sttId_f: EIMEVTYPEへ登録するSTTID_F
		3. OUT: O_sttId_t: EIMEVTYPEへ登録するSTTID_T
		4. OUT: O_duplexExist: EIMEVTYPEへ登録対象のSTTID_F,STTID_Tで登録
		                       されている「公開取消」イベントの件数
	------------------------------------------------------------------------*/
	procedure getRegistInfo(
		I_workflowId	in	number
		, O_sttId_f		out	number
		, O_sttId_t		out	number
		, O_duplexExist	out	number
	)
	is
	
	begin
		
		-- 「公開取消」イベント登録用_FromID取得
		select status.id
		  into O_sttId_f
		  from EIMSTTYPE status
		 where status.kind = CONS_STATUS_KIND_FROM
		   and status.workflow = I_workflowId;
		   
		-- 「公開取消」イベント登録用_ToID取得
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
		「公開取消」イベントタイプ登録処理
		・EIMEVTYPE, EIMEVTYPEOTHER、EIMNOTICEMAILへのinsertを実行する
		[引数]
		1. IN:  I_sttId_f: EIMEVTYPEへ登録するSTTID_F
		2. IN:  I_sttId_t: EIMEVTYPEへ登録するSTTID_T
	------------------------------------------------------------------------*/
	procedure insertEventType(
		  I_sttId_f		in	number
		, I_sttId_t		in	number
	)
	is
		eventId		number;
		mailId		number;
	begin
		-- PK(EVTID)を採番
		eventId := EIMID.nextval;
		-- PK(MID)を採番
		mailId  := EIMID.nextval;
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
		       
		-- EIMNOTICEMAIL(通知メール情報)へ登録
		insert into EIMNOTICEMAIL
		       values(
		          mailId
		        , eventId
		        , CONS_MAIL_TYPE
		        , CONS_MAIL_METHOD
		       );
		       
	end insertEventType;
	
begin
	/* ================================================================== */
	-- ドキュメント管理のワークフローに対してイベント「公開取消」を登録する
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
			insertEventType(
				sttId_f
			  , sttId_t
			);
			
		end if;
		
	end loop;
	
	commit;

exception
	when OTHERS then
		dbms_output.put_line('エラー発生： ' || sqlerrm(sqlcode));

end;
/
