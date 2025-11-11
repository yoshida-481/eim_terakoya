set serveroutput on
DECLARE
	
	--**************************************************************************
	--定数
	--**************************************************************************
	----------------------------------------------
	--ベースイベントタイプ
	----------------------------------------------
	--承認依頼
	CONS_BET_APPREQ		constant number := -14001;
	--承認
	CONS_BET_APPROVE	constant number := -14002;
	--公開
	CONS_BET_PUBLIC		constant number := -14003;
	--承認依頼取消
	CONS_BET_CANAPPREQ	constant number := -14004;
	--差戻し
	CONS_BET_SENDBACK	constant number := -14005;
	
	NOT_FOUND				constant number := -1;
	
	CONS_SYSTEMUSER			constant number := 1;
	
	CONS_ENTRYTYPE_USER		constant number := 1;
	CONS_ENTRYTYPE_GROUP	constant number := 2;
	CONS_ENTRYTYPE_ROLE		constant number := 3;
	CONS_ENTRYTYPE_CMP		constant number := 4;
	
	MAILSEND_NO				constant number := 3;

	--**************************************************************************
	--変数
	--**************************************************************************
	--テーブルレコード処理用の添え字
	idx	number := 0;
	cntEdit number := 0;
	rcode	number := 0;
	delStatusCnt	number := 0;
	
	--メール通知オブジェクトタイプに必要な属性タイプID
	objTypeId_MailNotification number := 0;
	attTypeId_Reply number := 0;
	attTypeId_NotificationOfTiming number := 0;
	attTypeId_ApprovalReqTiming number := 0;
	attTypeId_NotificationOfDest number := 0;
	attTypeId_Comment number := 0;
	
	wk_errmsg varchar2(2000);
	wk_errno varchar2(2000);
	wk_errmsgno varchar2(2000);
	
	--**************************************************************************
	--カーソル
	--**************************************************************************
	--移行テーブルから
	--移行対象のドキュメント・WF付きフォルダ、を取得
	--WF付きフォルダ配下のドキュメントは除く
	--13705,14619
	cursor c_target_obj is
	select X.* from IKOU_TARGET_OBJ X
	where
	1 = 1
	and exists( select 'X' from IKOU_TARGET_WORKFLOW Y where Y.workflow = X.workflow and Y.ikou_flg = 1)
	--DEBUG
	--and id = 244
	and ikou_flg = 0
	order by seq;
	
	--
	--EIMSTTYPE
	--
	cursor c_eimsttype(p_workflowId number) is
	with sttlist as
	(
		select
			EST.name as sttname
			,EST.id as sttid
			,level as step
			,EST.auto as auto
			,EST.kind as kind
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
	)
	select
		 X.sttid as sttid
		,X.step as step
		,(select OEST.kind from IKOU_OLD_EIMSTTYPE OEST where X.sttid = OEST.id) as oldkind
	from
	sttlist X
	order by X.step
	;
	--
	--EIMST、並び順も取得
	--
	cursor c_eimst(p_objId number, p_workflowId number) is
		select
		 ES.sid
		,ES.oid
		,ES.type as sttype
		,ES.cuser
		,ES.cdate
		,ES.muser
		,ES.mdate
		,ES.rev
		,ES.latest
		,(select STT.kind from EIMSTTYPE STT where STT.id = ES.type) as kind
		,(select OEST.kind from IKOU_OLD_EIMSTTYPE OEST where OEST.workflow = p_workflowId and ES.type = OEST.id) as oldkind
		,(select OEST.step from IKOU_OLD_EIMSTTYPE OEST where OEST.workflow = p_workflowId and ES.type = OEST.id) as step
		from
		eimst ES
		where
			ES.oid = p_objId
		order by ES.sid
		;
		
		--指定したステータスタイプIDのアサイン先エントリーを取得する
		cursor c_eimasentry(p_statustypeid number) is
		select
			 AEID
			,STTID
			,TYPE as ENTRYTYPE
			,ENTRY as ENTRYID
		from
		EIMASENTRY
		where
		sttid = p_statustypeid
		order by AEID
		;
	
	--重複を除いたユーザを取得する
	cursor c_distinctuser(p_userid_array EIMARRAY) is
	select
		distinct id as userid
	from
		EIMUSER
	where
		id in (select * from table(p_userid_array))
	order by userid
	;
	
	--引数のグループIDの子グループも含めたユーザを取得する
	cursor c_recurgroupuser(p_groupid number) is
	select
		 distinct id as userid
	from
	EIMGASGN GA
	where
	gid in (
		select EG.id
		from EIMGROUP EG
		start with EG.id = p_groupid
		connect by prior EG.id = EG.parent
	)
	order by userid
	;
	
	--引数のロールIDのユーザを取得する
	cursor c_roleuser(p_roleid number) is
	select
		 distinct id as userid
	from
	EIMRASGN RA
	where
	rid = p_roleid
	order by userid
	;
	--引数の複合グループIDのユーザを取得する
	cursor c_cmpuser(p_cmpid number) is
	select
		 distinct EU.id		as userid
	from
		EIMUSER EU, EIMGASGN EGA, EIMRASGN ERA
	where EGA.gid = (select gid from EIMCMP where id = p_cmpid)
		and ERA.rid = (select rid from EIMCMP where id = p_cmpid)
		and EGA.id = EU.id
		and ERA.id = EU.id
	order by userid
	;

	--
	--差戻しオブジェクトを取得
	--
	cursor c_sendback(p_objId number) is
	select
	*
	from
	eimobj
	where
	type = (select id from eimobjtype where name = '差戻し')
	and name in (select sid from eimst where oid = p_objId)
	order by name
	;
	
	--
	--引数のオブジェクトタイプ名とオブジェクトIDで属性値を取得する
	--
	cursor c_attrvalue(p_objId number, p_objTypeName varchar2) is
	with target as
	(
		select
		id
		from
		eimobj
		where
		type = (select id from eimobjtype where name = p_objtypeName)
		and name = to_char(p_objId)
	)
	select
	X.id
	,(select Y.name from eimobjtype Y where Y.id = (select Z.type from eimobj Z where Z.id = X.id) ) as typename
	,(select Y.name from eimattr Y where Y.id = X.type) as attrname
	,X.type
	,X.key
	,X.int_value as int_value
	,X.str_value as str_value
	,X.date_value as date_value
	,X.text_value as text_value
	from
	(
	select
	id,type,key,value as int_value,null as str_value,null as date_value,null as text_value
	from eimobjint
	where id in (select id from target)
	union
	select
	id,type,key,null as int_value,value as str_value,null as date_value,null as text_value
	from eimobjstr
	where id in (select id from target)
	union
	select
	id,type,key,null as int_value,null as str_value,value as date_value,null as text_value
	from eimobjdate
	where id in (select id from target)
	union
	select
	id,type,key,null as int_value,null as str_value,null as date_value,to_char(value) as text_value
	from eimobjtext
	where id in (select id from target)
	) X
	order by id , attrname , key
	;
	
	--
	--FROMステータスタイプID、TOステータスタイプID、ベースイベントタイプIDでイベントタイプIDを取得する
	--
	cursor c_eventtype
	(p_f_statustypeid number, p_t_statustypeid number, p_baseeventtypeid number) is
	select
	 evtid
	,name
	,sttid_f
	,sttid_t
	,bevtid
	,guardid
	,seq
	from
		EIMEVTYPE
	where
		sttid_f = p_f_statustypeid
	and sttid_t = p_t_statustypeid
	and bevtid = p_baseeventtypeid
	;
	
	--
	--指定オブジェクトに紐づく承認依頼者・承認者・差戻しのデータを取得する
	--
	cursor c_wf_rireki01(p_objId number) is
	with target as
	(
		select
		 X.id
		,(select Y.name from eimobjtype Y where Y.id = (select Z.type from eimobj Z where Z.id = X.id) ) as typename
		,X.name
		from
		eimobj X
		where
		X.type in (select id from eimobjtype where name in ('承認者','承認依頼者','差戻し'))
		and X.name in (select to_char(sid) from eimst where oid = p_objId)
	)
	select
	 id
	,typename
	,to_number(name) as sid
	,DECODE(typename,'承認者',syouninuser,DECODE(typename,'承認依頼者',iraiuser,DECODE(typename,'差戻し',sendbackuser))) as cuser
	,DECODE(typename,'承認者',syounindate) as cdate
	,wfcomment
	from
	(
		select
		 X.id
		,X.typename
		,X.name
		,(select Y.value from eimobjint Y where key = 0 and Y.id = X.id and type = (select id from eimattr where name = '承認依頼者')) as iraiuser
		,(select Y.value from eimobjint Y where key = 0 and Y.id = X.id and type = (select id from eimattr where name = '承認者')) as syouninuser
		,(select Y.value from eimobjint Y where key = 0 and Y.id = X.id and type = (select id from eimattr where name = '差戻しユーザ')) as sendbackuser
		,(select Y.value from eimobjdate Y where key = 0 and Y.id = X.id and type = (select id from eimattr where name = '承認日')) as syounindate
		,(select Y.value from eimobjstr Y where key = 0 and Y.id = X.id and type = (select id from eimattr where name = 'コメント')) as wfcomment
		from
		target X
	)
	order by typename, sid, cdate
	;

	--
	--指定オブジェクトに紐づく被承認依頼者・再承認依頼先のデータを取得する
	--
	cursor c_wf_rireki02(p_objId number) is
	with target as
	(
		select
		 X.id
		,(select Y.name from eimobjtype Y where Y.id = (select Z.type from eimobj Z where Z.id = X.id) ) as typename
		,X.name
		from
		eimobj X
		where
		X.type in (select id from eimobjtype where name in ('被承認依頼者','再承認依頼先'))
		and X.name in (select to_char(sid) from eimst where oid = p_objId)
	)
	select
	 id
	,typename
	,to_number(name) as sid
	,DECODE(typename,'被承認依頼者',to_number(hientrytype),DECODE(typename,'再承認依頼先',to_number(saientrytype))) as entrytype
	,DECODE(typename,'被承認依頼者',to_number(hientryid),DECODE(typename,'再承認依頼先',to_number(saientryid))) as entryid
	from
	(
		select
		 X.id
		,X.typename
		,X.name
		,(select substr(Y.value,1,1) from eimobjstr Y where key = 0 and Y.id = X.id and type = (select id from eimattr where name = '被承認依頼者種別：ID')) as hientrytype
		,(select substr(Y.value,3) from eimobjstr Y where key = 0 and Y.id = X.id and type = (select id from eimattr where name = '被承認依頼者種別：ID')) as hientryid
		,(select substr(Y.value,1,1) from eimobjstr Y where key = 0 and Y.id = X.id and type = (select id from eimattr where name = '再承認依頼先種別：ID')) as saientrytype
		,(select substr(Y.value,3) from eimobjstr Y where key = 0 and Y.id = X.id and type = (select id from eimattr where name = '再承認依頼先種別：ID')) as saientryid
		from
		target X
	)
	;

	--**************************************************************************
	--配列
	--**************************************************************************
	--
	--EIMSTTYPE
	--
	type type_rec_eimsttype is record
	(
		  sttid		eimsttype.id%type
		 ,step		number
		 ,oldkind	number
	);
	type type_tab_eimsttype is table of type_rec_eimsttype index by binary_integer;
	tab_eimsttype type_tab_eimsttype;
	
	--
	--EIMST
	--
	type type_rec_eimst is record
	(
		 sid		eimst.sid%type
		,oid		eimst.oid%type
		,sttype		eimst.type%type
		,cuser		eimst.cuser%type
		,cdate		eimst.cdate%type
		,muser		eimst.muser%type
		,mdate		eimst.mdate%type
		,rev		eimst.rev%type
		,latest		eimst.latest%type
		,kind		eimsttype.kind%type
		,oldkind	eimsttype.kind%type
		,step		number
	);
	type type_tab_eimst is table of type_rec_eimst index by binary_integer;
	--１つのオブジェクトに紐づくステータス(グローバル扱い)
	tab_eimst type_tab_eimst;
	--削除SID格納用、EIMST削除後再取得用
	tab_eimst_del	type_tab_eimst;
	
	--
	--承認依頼者・承認者・被承認依頼者・差戻し 格納用配列
	--
	--承認依頼者・承認者・差戻し
	type type_rec_syounin is record
	(
		 id			number
		,typename	eimobjtype.name%type
		,sid		eimst.sid%type
		,cuser		number
		,cdate		date
		,wfcomment	eimobjstr.value%type
	);
	type type_tab_syounin is table of type_rec_syounin index by binary_integer;
	--承認依頼者
	tab_syouninirai type_tab_syounin;
	--承認者
	tab_syouninsya type_tab_syounin;
	--承認者(同じSIDをもった)格納用配列
	tab_syouninsya_samesid type_tab_syounin;
	--差戻し
	tab_sendback type_tab_syounin;
	
	--被承認依頼者、再承認依頼先
	type type_rec_hiandsaisyounin is record
	(
		 id			number
		,typename	eimobjtype.name%type
		,sid		eimst.sid%type
		,entrytype	number
		,entryid	number
	);
	type type_tab_hiandsaisyounin is table of type_rec_hiandsaisyounin index by binary_integer;
	--被承認依頼者
	tab_hisyouninirai type_tab_hiandsaisyounin;
	--被承認依頼者(同じSIDをもった)格納用配列
	tab_hisyouninirai_samesid type_tab_hiandsaisyounin;
	--再承認依頼先
	tab_saiiraisaki type_tab_hiandsaisyounin;
	--再承認依頼先(同じSIDをもった)格納用配列
	tab_saiiraisaki_samesid type_tab_hiandsaisyounin;
	
	--
	--イベントタイプ格納用
	--
	type type_rec_eventtype is record
	(
		 evtid		eimevtype.evtid%type
		,name		eimevtype.name%type
		,sttid_f	eimevtype.sttid_f%type
		,sttid_t	eimevtype.sttid_t%type
		,bevtid		eimevtype.bevtid%type
		,guardid	eimevtype.guardid%type
		,seq		eimevtype.seq%type
	);
	type type_tab_eventtype is table of type_rec_eventtype index by binary_integer;
	tab_eventtype type_tab_eventtype;
	
	--
	--イベント格納用
	--
	type type_rec_event is record
	(
		 evid		eimev.evid%type
		,cuser		eimev.cuser%type
	);
	type type_tab_event is table of type_rec_event index by binary_integer;
	tab_event type_tab_event;
	
	--アサイン先格納用
	type type_rec_as is record
	(
		 asid		number
		,sttid		number
		,entry		number
	);
	type type_tab_as is table of type_rec_as index by binary_integer;
	tab_as type_tab_as;
	
	--アサイン先予定格納用
	type type_rec_aspln is record
	(
		 apid		number
		,sttid		number
		,entry		number
	);
	type type_tab_aspln is table of type_rec_aspln index by binary_integer;
	tab_aspln type_tab_aspln;

	--アサイン先エントリー格納用
	type type_rec_asentry is record
	(
		 aeid		number
		,sttid		number
		,entrytype	number
		,entryid	number
	);
	type type_tab_asentry is table of type_rec_asentry index by binary_integer;
	tab_asentry type_tab_asentry;
	
	--ユーザ格納用
	userid_array			EIMARRAY;
	
	--**************************************************************************
	--プロシージャ/ファンクション
	--**************************************************************************
	--差戻しで遷移した不必要なステータスを削除する
	--引数がないのはグローバル変数を使用するため、処理を分割するためにプロシージャ化
	PROCEDURE del_senbackstatus(p_objId IN number)
	IS
		findIdx	number := 0;
		tmpIdx	number := 0;
		curStep	number := 0;
	BEGIN
		--差戻しオブジェクトの数だけループ
		FOR r_sendback IN c_sendback(p_objId) LOOP
			--オブジェクトIDに紐づいているEIMST
			FOR i IN 0..tab_eimst.count-1 LOOP
				IF to_number(r_sendback.name) = tab_eimst(i).sid THEN
					findIdx := i;
					tmpIdx := findIdx;
					tmpIdx := tmpIdx - 1;
					--辿る
					WHILE tmpIdx > 0 LOOP
						IF tab_eimst(tmpIdx).step < tab_eimst(tmpIdx - 1).step THEN
							tab_eimst_del(delStatusCnt).sid := tab_eimst(tmpIdx).sid;
							delStatusCnt := delStatusCnt + 1;
						ELSE
							EXIT;
						END IF;
						tmpIdx := tmpIdx - 1;
					END LOOP;
				END IF;
			END LOOP;
		END LOOP;
		
		--EIMSTとEIMVERを削除する
		FOR i IN 0..tab_eimst_del.count-1 LOOP
			delete from EIMST where sid = tab_eimst_del(i).sid;
			delete from EIMSTVER where sid = tab_eimst_del(i).sid;
		END LOOP;
		
		--EIMSTのREV列とLATEST列を再構築する
		update eimst X
		set (X.rev,X.latest) = (
			select
			Y.newrev,Y.newlatest
			from
			(
				select
				sid,type,rev,latest,newrev,maxnewrev
				,DECODE(maxnewrev,newrev,1,0) as newlatest
				from
				(
					select
					sid,type,rev,latest,newrev
					,max(newrev) over (partition by type) as maxnewrev
					from
					(
						select
						sid,type,rev,latest
						,row_number() over (partition by type order by sid) - 1 as newrev
						from
						eimst
						where
						oid = p_objId
					)
				)
			) Y
			where
			Y.sid = X.sid
		)
		where
		oid = p_objId
		;
		
	END;
	
	--
	--メール通知オブジェクトを作成する
	--
	PROCEDURE cre_mailnotification(p_objId IN number)
	IS
		--複数値属性値のKEY列なので1からスタートする
		keyIdx		number := 1;
		mailObjId	number := 0;
		tmpObjId	number := 0;
		tmpTiming	number := 0;
		irai_timingFlg			boolean := false;
		irai_jusinkakuninFlg	boolean := false;
		koukai_timingFlg		boolean := false;
		jusinkakuninCnt	number := 0;
	BEGIN
		--メール通知オブジェクトを作成する
		insert into EIMOBJ values(EIMID.nextval, objTypeId_MailNotification, to_char(p_objId), 0, 1, 1, sysdate, 1, sysdate, null, null, 0, 0, 0, 0);
		select EIMID.currval into mailObjId from dual;
		insert into EIMVER values(EIMID.nextval, mailObjId);
		
		--承認依頼
		FOR r_attrvalue IN c_attrvalue(p_objId,'承認依頼') LOOP
			IF r_attrvalue.attrname = '受信確認' THEN
				--メール通知.受信確認
				insert into EIMOBJINT values(mailObjId, attTypeId_Reply, 0, r_attrvalue.int_value);
				irai_jusinkakuninFlg := true;
			END IF;
			IF r_attrvalue.attrname = '通知タイミング' THEN
				--メール通知.承認依頼通知タイミング
				insert into EIMOBJINT values(mailObjId, attTypeId_ApprovalReqTiming, 0, r_attrvalue.int_value);
				irai_timingFlg := true;
			END IF;
		END LOOP;
		
		--承認依頼オブジェクトが取得できず、受信確認属性値が取得できない場合
		IF irai_jusinkakuninFlg = false THEN
			select count(id) into jusinkakuninCnt from EIMOBJ where name like to_char(p_objId) || '.%';
			IF jusinkakuninCnt > 0 THEN
				--受信確認あり=1に設定する
				insert into EIMOBJINT values(mailObjId, attTypeId_Reply, 0, 1);
			ELSE
				--受信確認なし=0に設定する
				insert into EIMOBJINT values(mailObjId, attTypeId_Reply, 0, 0);
			END IF;
		END IF;
		--承認依頼オブジェクトが取得できず、通知タイミング属性値が取得できない場合
		IF irai_timingFlg = false THEN
			--メール送信なし=3に設定する
			insert into EIMOBJINT values(mailObjId, attTypeId_ApprovalReqTiming, 0, MAILSEND_NO);
		END IF;
		
		--公開通知
		FOR r_attrvalue IN c_attrvalue(p_objId,'公開通知') LOOP
			--公開通知オブジェクトのオブジェクトID
			tmpObjId := r_attrvalue.id;
			IF r_attrvalue.attrname = '通知タイミング' THEN
				IF r_attrvalue.int_value = 2 THEN
					tmpTiming := 1;
				ELSE
					tmpTiming := r_attrvalue.int_value;
				END IF;
				--メール通知.公開通知タイミング
				insert into EIMOBJINT values(mailObjId, attTypeId_NotificationOfTiming, 0, tmpTiming);
				koukai_timingFlg := true;
			END IF;
		END LOOP;
		
		--公開通知オブジェクトに紐づく通知先オブジェクト
		FOR r_attrvalue IN c_attrvalue(tmpObjId,'通知先') LOOP
			IF r_attrvalue.attrname = '通知先種別：ID' THEN
				--メール通知.公開通知送信先
				insert into EIMOBJSTR values(mailObjId, attTypeId_NotificationOfDest, keyIdx, r_attrvalue.str_value);
				keyIdx := keyIdx + 1;
			END IF;
		END LOOP;
		
		--公開通知オブジェクトが取得できず、通知タイミング属性値が取得できない場合
		IF koukai_timingFlg = false THEN
			--メール送信なし=3に設定する
			insert into EIMOBJINT values(mailObjId, attTypeId_NotificationOfTiming, 0, MAILSEND_NO);
		END IF;
		
	END;

	--
	--指定したオブジェクトに紐づく全てのステータスIDで承認者・被承認依頼者・承認依頼者・差戻しオブジェクトを取得して
	--それぞれの配列に格納する
	--
	PROCEDURE sel_wf_rireki(p_objId IN number)
	IS
		syouniniraiIdx number := 0;
		syouninsyaIdx number := 0;
		sendbackIdx number := 0;
		hisyouniniraiIdx number := 0;
		saiiraisakiIdx number := 0;
	BEGIN
		--承認依頼者・承認者・被承認依頼者・承認者(ST指定)・再承認依頼先・再承認依頼先(ST指定)・差戻し 格納用配列をクリア
		tab_syouninirai.delete;
		tab_syouninsya.delete;
		tab_hisyouninirai.delete;
		tab_saiiraisaki.delete;
		tab_sendback.delete;
		FOR r_wf_rireki IN c_wf_rireki01(p_objId) LOOP
			IF r_wf_rireki.typename = '承認依頼者' THEN
				tab_syouninirai(syouniniraiIdx) := r_wf_rireki;
				syouniniraiIdx := syouniniraiIdx + 1;
			ELSIF r_wf_rireki.typename = '承認者' THEN
				tab_syouninsya(syouninsyaIdx) := r_wf_rireki;
				syouninsyaIdx := syouninsyaIdx + 1;
			ELSIF r_wf_rireki.typename = '差戻し' THEN
				tab_sendback(sendbackIdx) := r_wf_rireki;
				sendbackIdx := sendbackIdx + 1;
			END IF;
		END LOOP;
		FOR r_wf_rireki IN c_wf_rireki02(p_objId) LOOP
			IF r_wf_rireki.typename = '被承認依頼者' THEN
				tab_hisyouninirai(hisyouniniraiIdx) := r_wf_rireki;
				hisyouniniraiIdx := hisyouniniraiIdx + 1;
			END IF;
			IF r_wf_rireki.typename = '再承認依頼先' THEN
				tab_saiiraisaki(saiiraisakiIdx) := r_wf_rireki;
				saiiraisakiIdx := saiiraisakiIdx + 1;
			END IF;
		END LOOP;
	END;
	
	--次のステータスタイプのステータスタイプIDを取得する
	FUNCTION get_nextstatustypeid
	(p_statustypeid IN number)
	RETURN number
	IS
	BEGIN
		FOR i IN 0..tab_eimsttype.count-1 LOOP
			IF tab_eimsttype(i).sttid = p_statustypeid THEN
				IF tab_eimsttype.exists(i+1) THEN
					return tab_eimsttype(i+1).sttid;
				END IF;
			END IF;
		END LOOP;
		return 0;
	END;
	
	--引数のステータスタイプの移行前のKIND値を取得する
	FUNCTION get_kind
	(p_statustypeid IN number)
	RETURN number
	IS
		r_kind number := 0;
	BEGIN
		select ESTT.kind into r_kind from IKOU_OLD_EIMSTTYPE ESTT where id = p_statustypeid;
		return r_kind;
	END;
	
	--引数のステータスIDの再承認依頼先オブジェクトを配列に格納する
	PROCEDURE set_saiiraisaki_samesid_array(p_statusid IN number)
	IS
		sameidIdx number := 0;
	BEGIN
		tab_saiiraisaki_samesid.delete;
		FOR i IN 0..tab_saiiraisaki.count-1 LOOP
			IF tab_saiiraisaki(i).sid = p_statusid THEN
				tab_saiiraisaki_samesid(sameidIdx) := tab_saiiraisaki(i);
				sameidIdx := sameidIdx + 1;
			END IF;
		END LOOP;
	END;
	
	--引数のステータスIDの承認者オブジェクトを配列に格納する
	PROCEDURE set_syouninsya_samesid_array(p_statusid IN number)
	IS
		sameidIdx number := 0;
	BEGIN
		tab_syouninsya_samesid.delete;
		FOR i IN 0..tab_syouninsya.count-1 LOOP
			IF tab_syouninsya(i).sid = p_statusid THEN
				tab_syouninsya_samesid(sameidIdx) := tab_syouninsya(i);
				sameidIdx := sameidIdx + 1;
			END IF;
		END LOOP;
	END;
	
	--
	--引数のステータスIDの被承認依頼者オブジェクトの数を返却する
	--
	FUNCTION get_hisyouniniraiCnt
	(p_statusid IN number)
	RETURN number
	IS
		findCnt number := 0;
	BEGIN
		FOR i IN 0..tab_hisyouninirai.count-1 LOOP
			IF tab_hisyouninirai(i).sid = p_statusid THEN
				findCnt := findCnt + 1;
			END IF;
		END LOOP;
		return findCnt;
	END;
	
	--
	--引数のステータスIDの被承認依頼者オブジェクトを配列に格納する
	--
	PROCEDURE set_hiirai_samesid_array
	(p_statusid IN number)
	IS
		sameidIdx number := 0;
	BEGIN
		tab_hisyouninirai_samesid.delete;
		FOR i IN 0..tab_hisyouninirai.count-1 LOOP
			IF tab_hisyouninirai(i).sid = p_statusid THEN
				tab_hisyouninirai_samesid(sameidIdx) := tab_hisyouninirai(i);
				sameidIdx := sameidIdx + 1;
			END IF;
		END LOOP;
	END;
	
	--
	--引数のステータスIDの承認者オブジェクトの数を返却する
	--
	FUNCTION get_syouninsyaCnt
	(p_statusid IN number)
	RETURN number
	IS
		findCnt number := 0;
	BEGIN
		FOR i IN 0..tab_syouninsya.count-1 LOOP
			IF tab_syouninsya(i).sid = p_statusid THEN
				findCnt := findCnt + 1;
			END IF;
		END LOOP;
		return findCnt;
	END;

	--
	--差戻しデータ格納用配列から引数のステータスIDで差戻しオブジェクトを探して
	--該当のインデックスを返却する
	--
	FUNCTION sel_sendbackIdx
	(p_statusid IN number)
	RETURN number
	IS
	BEGIN
		FOR i IN 0..tab_sendback.count-1 LOOP
			IF tab_sendback(i).sid = p_statusid THEN
				return i;
			END IF;
		END LOOP;
		return NOT_FOUND;
	END;
	
	--
	--イベントタイプIDを取得して返却する
	--
	FUNCTION sel_eventtypeid
	(p_f_statustypeid IN number, p_t_statustypeid IN number, p_baseeventtypeid IN number)
	RETURN number
	IS
		r_eventtypeid number := 0;
	BEGIN
		select
			evtid
		into
			r_eventtypeid
		from
			EIMEVTYPE
		where
			sttid_f = p_f_statustypeid
		and sttid_t = p_t_statustypeid
		and bevtid = p_baseeventtypeid
		;
		return r_eventtypeid;
	END;
	
	--
	--アサイン先予定リレーションを作成する
	--引数がないのはグローバル変数を使用するため、処理を分割するためにプロシージャ化
	--
	PROCEDURE cre_eimasplnrel
	IS
		asentryIdx	number := 0;
		userCnt		number := 0;
	BEGIN
		--DBMS_OUTPUT.PUT_LINE('cre_eimasplnrel');
		IF tab_aspln.count = 0 THEN
			return;
		END IF;
		
		--DBMS_OUTPUT.PUT_LINE('エントリー取得前 STTID=' || tab_aspln(0).sttid);
		
		--アサイン先エントリーを取得して配列に格納する
		--アサイン先予定の配列のsttidは全て同じ値が入っているため0番目で取得する
		tab_asentry.delete;
		FOR r_eimasentry IN c_eimasentry(tab_aspln(0).sttid) LOOP
			tab_asentry(asentryIdx) := r_eimasentry;
			asentryIdx := asentryIdx + 1;
		END LOOP;
		
		--DBMS_OUTPUT.PUT_LINE('エントリー取得後COUNT=' || tab_asentry.count);
		
		--アサイン先予定の数だけループ
		FOR i IN 0..tab_aspln.count-1 LOOP
			--ステータスタイプIDでアサイン先エントリーを取得する
			FOR ii IN 0..tab_asentry.count-1 LOOP
				userCnt := 0;
				IF tab_asentry(ii).entrytype = CONS_ENTRYTYPE_USER THEN
					--DBMS_OUTPUT.PUT_LINE('USER=' || tab_asentry(ii).entryid);
					IF tab_aspln(i).entry = tab_asentry(ii).entryid THEN
						insert into EIMASPLNREL values(tab_aspln(i).apid, tab_asentry(ii).aeid);
					END IF;
				ELSIF tab_asentry(ii).entrytype = CONS_ENTRYTYPE_GROUP THEN
					--DBMS_OUTPUT.PUT_LINE('GROUP=' || tab_asentry(ii).entryid);
					select count(id) into userCnt from EIMGASGN
					where gid = tab_asentry(ii).entryid and id = tab_aspln(i).entry;
					
					IF userCnt > 0 THEN
						insert into EIMASPLNREL values(tab_aspln(i).apid, tab_asentry(ii).aeid);
					END IF;
				ELSIF tab_asentry(ii).entrytype = CONS_ENTRYTYPE_ROLE THEN
					--DBMS_OUTPUT.PUT_LINE('ROLE=' || tab_asentry(ii).entryid);
					select count(id) into userCnt from EIMRASGN
					where rid = tab_asentry(ii).entryid and id = tab_aspln(i).entry;
					
					IF userCnt > 0 THEN
						insert into EIMASPLNREL values(tab_aspln(i).apid, tab_asentry(ii).aeid);
					END IF;
				ELSIF tab_asentry(ii).entrytype = CONS_ENTRYTYPE_CMP THEN
					--DBMS_OUTPUT.PUT_LINE('CMP=' || tab_asentry(ii).entryid);
					select count(EU.id) into userCnt from EIMUSER EU, EIMGASGN EGA, EIMRASGN ERA
					where
						EGA.gid = (select gid from EIMCMP where id = tab_asentry(ii).entryid)
					and ERA.rid = (select rid from EIMCMP where id = tab_asentry(ii).entryid)
					and EGA.id = EU.id
					and ERA.id = EU.id
					and EU.id = tab_aspln(i).entry;
					
					IF userCnt > 0 THEN
						insert into EIMASPLNREL values(tab_aspln(i).apid, tab_asentry(ii).aeid);
					END IF;
				END IF;
			END LOOP;
		END LOOP;
		
	END;
	
	--
	--アサイン先リレーションを作成する
	--引数がないのはグローバル変数を使用するため、処理を分割するためにプロシージャ化
	--
	PROCEDURE cre_eimasrel
	IS
		asentryIdx	number := 0;
		userCnt		number := 0;
	BEGIN
		--DBMS_OUTPUT.PUT_LINE('cre_eimasrel');
		IF tab_as.count = 0 THEN
			return;
		END IF;
		
		--DBMS_OUTPUT.PUT_LINE('エントリー取得前 STTID=' || tab_as(0).sttid);
		
		--アサイン先エントリーを取得して配列に格納する
		--アサイン先の配列のsttidは全て同じ値が入っているため0番目で取得する
		tab_asentry.delete;
		FOR r_eimasentry IN c_eimasentry(tab_as(0).sttid) LOOP
			tab_asentry(asentryIdx) := r_eimasentry;
			asentryIdx := asentryIdx + 1;
		END LOOP;
		
		--DBMS_OUTPUT.PUT_LINE('エントリー取得後COUNT=' || tab_asentry.count);
		
		--アサイン先の数だけループ
		FOR i IN 0..tab_as.count-1 LOOP
			--ステータスタイプIDでアサイン先エントリーを取得する
			FOR ii IN 0..tab_asentry.count-1 LOOP
				userCnt := 0;
				IF tab_asentry(ii).entrytype = CONS_ENTRYTYPE_USER THEN
					--DBMS_OUTPUT.PUT_LINE('USER=' || tab_asentry(ii).entryid);
					IF tab_as(i).entry = tab_asentry(ii).entryid THEN
						insert into EIMASREL values(tab_as(i).asid, tab_asentry(ii).aeid);
					END IF;
				ELSIF tab_asentry(ii).entrytype = CONS_ENTRYTYPE_GROUP THEN
					--DBMS_OUTPUT.PUT_LINE('GROUP=' || tab_asentry(ii).entryid);
					select count(id) into userCnt from EIMGASGN
					where gid = tab_asentry(ii).entryid and id = tab_as(i).entry;
					
					IF userCnt > 0 THEN
						insert into EIMASREL values(tab_as(i).asid, tab_asentry(ii).aeid);
					END IF;
				ELSIF tab_asentry(ii).entrytype = CONS_ENTRYTYPE_ROLE THEN
					--DBMS_OUTPUT.PUT_LINE('ROLE=' || tab_asentry(ii).entryid);
					select count(id) into userCnt from EIMRASGN
					where rid = tab_asentry(ii).entryid and id = tab_as(i).entry;
					
					IF userCnt > 0 THEN
						insert into EIMASREL values(tab_as(i).asid, tab_asentry(ii).aeid);
					END IF;
				ELSIF tab_asentry(ii).entrytype = CONS_ENTRYTYPE_CMP THEN
					--DBMS_OUTPUT.PUT_LINE('CMP=' || tab_asentry(ii).entryid);
					select count(EU.id) into userCnt from EIMUSER EU, EIMGASGN EGA, EIMRASGN ERA
					where
						EGA.gid = (select gid from EIMCMP where id = tab_asentry(ii).entryid)
					and ERA.rid = (select rid from EIMCMP where id = tab_asentry(ii).entryid)
					and EGA.id = EU.id
					and ERA.id = EU.id
					and EU.id = tab_as(i).entry;
					
					IF userCnt > 0 THEN
						insert into EIMASREL values(tab_as(i).asid, tab_asentry(ii).aeid);
					END IF;
				END IF;
			END LOOP;
		END LOOP;
		
	END;
	
	--
	--イベントを作成する
	--
	PROCEDURE cre_event(p_objId IN number)
	IS
		--次のステータスが存在する場合はtrue
		existsNextFlg boolean := false;
		eventtypeid	number := 0;
		eventid		number := 0;
		eventCnt	number := 0;
		nextstatustypeid	number := 0;
		comment		varchar2(2000) := null;
		sendbackuser	number := 0;
		hisyouniniraiCnt	number := 0;
		userIdx		number := 1;
		userCnt		number := 0;
		userid		number := 0;
		asIdx		number := 0;
		asignid		number := 0;
		apIdx		number := 0;
		asignplnid		number := 0;
	BEGIN
		
		FOR i IN 0..tab_eimst.count-1 LOOP
			existsNextFlg := false;
			IF tab_eimst.exists(i+1) THEN
				existsNextFlg := true;
			END IF;
			--DBMS_OUTPUT.PUT_LINE('ステータス:' ||tab_eimst(i).sid);
			IF existsNextFlg = true THEN
			--過去のステータスの場合
				
				--■承認依頼　編集中⇒全員承認、編集中⇒一人承認
				IF tab_eimst(i).oldkind = 1 AND tab_eimst(i).step < tab_eimst(i+1).step AND (tab_eimst(i+1).oldkind = 2 OR tab_eimst(i+1).oldkind = 3) THEN
					eventtypeid := sel_eventtypeid(tab_eimst(i).sttype, tab_eimst(i+1).sttype, CONS_BET_APPREQ);
					--承認依頼のイベントを作成する
					FOR ii IN 0..tab_syouninirai.count-1 LOOP
						IF tab_syouninirai(ii).sid = tab_eimst(i+1).sid THEN
							insert into EIMEV values(EIMID.nextval, p_objId, eventtypeid, tab_eimst(i).sid, tab_eimst(i+1).sid, tab_syouninirai(ii).cuser, tab_eimst(i+1).cdate);
							comment := tab_syouninirai(ii).wfcomment;
							IF comment is not null THEN
								select EIMID.currval into eventid from dual;
								insert into EIMEVSTR values(eventid, attTypeId_Comment, 0, comment);
							END IF;
						END IF;
					END LOOP;
					--アサイン先・アサイン先予定は作成しない
				
				--■(一人承認)承認：一人承認⇒全員承認、一人承認⇒一人承認、一人承認⇒公開処理中
				ELSIF tab_eimst(i).oldkind = 3 AND tab_eimst(i).step < tab_eimst(i+1).step AND (tab_eimst(i+1).oldkind = 2 OR tab_eimst(i+1).oldkind = 3 OR tab_eimst(i+1).oldkind = 4) THEN
					--DBMS_OUTPUT.PUT_LINE(tab_eimst(i).sttype || ':' || tab_eimst(i+1).sttype || ':' || CONS_BET_APPROVE);
					eventtypeid := sel_eventtypeid(tab_eimst(i).sttype, tab_eimst(i+1).sttype, CONS_BET_APPROVE);
					
					--アサイン先予定を削除する
					delete from EIMASPLN where OID = p_objId and STTID = tab_eimst(i).sttype;
					FOR ii IN 0..tab_syouninsya.count-1 LOOP
					--承認者は一人だけなので１回しかループしない
						IF tab_syouninsya(ii).sid = tab_eimst(i).sid THEN
							--承認のイベントを作成する
							insert into EIMEV values(EIMID.nextval, p_objId, eventtypeid, tab_eimst(i).sid, tab_eimst(i+1).sid, tab_syouninsya(ii).cuser, tab_syouninsya(ii).cdate);
							comment := tab_syouninsya(ii).wfcomment;
							select EIMID.currval into eventid from dual;
							IF comment is not null THEN
								insert into EIMEVSTR values(eventid, attTypeId_Comment, 0, comment);
							END IF;
							--アサイン先予定を作成する
							insert into EIMASPLN values(EIMID.nextval, p_objId, tab_eimst(i).sttype, CONS_ENTRYTYPE_USER, tab_syouninsya(ii).cuser);
							--アサイン先を作成する
							insert into EIMAS values(EIMID.nextval, tab_eimst(i).sid, CONS_ENTRYTYPE_USER, tab_syouninsya(ii).cuser, eventid);
						END IF;
					END LOOP;
					
				--■(一人承認)承認依頼取消：一人承認⇒全員承認、一人承認⇒一人承認、一人承認⇒編集中
				ELSIF tab_eimst(i).oldkind = 3 AND tab_eimst(i).step > tab_eimst(i+1).step AND sel_sendbackIdx(tab_eimst(i+1).sid) = NOT_FOUND AND (tab_eimst(i+1).oldkind = 1 OR tab_eimst(i+1).oldkind = 2 OR tab_eimst(i+1).oldkind = 3) THEN
					eventtypeid := sel_eventtypeid(tab_eimst(i).sttype, tab_eimst(i+1).sttype, CONS_BET_CANAPPREQ);
					--承認依頼取消のイベントを作成する
					insert into EIMEV values(EIMID.nextval, p_objId, eventtypeid, tab_eimst(i).sid, tab_eimst(i+1).sid, tab_eimst(i+1).cuser, tab_eimst(i+1).cdate);
					select EIMID.currval into eventid from dual;
					--一人承認に依頼したとき未承認で承認依頼取消を行うと承認者の情報が残らないので移行仕様としてsystemユーザを承認依頼先にしたこととする
					--アサイン先予定を削除する
					delete from EIMASPLN where OID = p_objId and STTID = tab_eimst(i).sttype;
					--アサイン先予定を作成する
					insert into EIMASPLN values(EIMID.nextval, p_objId, tab_eimst(i).sttype, CONS_ENTRYTYPE_USER, CONS_SYSTEMUSER);
					--アサイン先を作成する
					insert into EIMAS values(EIMID.nextval, tab_eimst(i).sid, CONS_ENTRYTYPE_USER, CONS_SYSTEMUSER, 0);
				--■(一人承認)差戻し：一人承認⇒編集中
				ELSIF tab_eimst(i).oldkind = 3 AND tab_eimst(i).step > tab_eimst(i+1).step AND sel_sendbackIdx(tab_eimst(i+1).sid) <> NOT_FOUND AND (tab_eimst(i+1).oldkind = 1) THEN
					eventtypeid := sel_eventtypeid(tab_eimst(i).sttype, tab_eimst(i+1).sttype, CONS_BET_SENDBACK);
					--差戻しのイベントを作成する
					sendbackuser := tab_sendback(sel_sendbackIdx(tab_eimst(i+1).sid)).cuser;
					insert into EIMEV values(EIMID.nextval, p_objId, eventtypeid, tab_eimst(i).sid, tab_eimst(i+1).sid, sendbackuser, tab_eimst(i+1).cdate);
					comment := tab_sendback(sel_sendbackIdx(tab_eimst(i+1).sid)).wfcomment;
					select EIMID.currval into eventid from dual;
					IF comment is not null THEN
						insert into EIMEVSTR values(eventid, attTypeId_Comment, 0, comment);
					END IF;
					--アサイン先予定を削除する
					delete from EIMASPLN where OID = p_objId and STTID = tab_eimst(i).sttype;
					--アサイン先予定を作成する
					insert into EIMASPLN values(EIMID.nextval, p_objId, tab_eimst(i).sttype, CONS_ENTRYTYPE_USER, sendbackuser);
					--アサイン先を作成する
					insert into EIMAS values(EIMID.nextval, tab_eimst(i).sid, CONS_ENTRYTYPE_USER, sendbackuser, eventid);
				--■(全員承認)承認：一人承認⇒全員承認、一人承認⇒一人承認、一人承認⇒公開処理中
				ELSIF tab_eimst(i).oldkind = 2 AND tab_eimst(i).step < tab_eimst(i+1).step AND (tab_eimst(i+1).oldkind = 2 OR tab_eimst(i+1).oldkind = 3 OR tab_eimst(i+1).oldkind = 4) THEN
					eventtypeid := sel_eventtypeid(tab_eimst(i).sttype, tab_eimst(i+1).sttype, CONS_BET_APPROVE);
					--被承認依頼者の数を取得する
					hisyouniniraiCnt := get_hisyouniniraiCnt(tab_eimst(i).sid);
					set_syouninsya_samesid_array(tab_eimst(i).sid);
					--DBMS_OUTPUT.PUT_LINE('被承認依頼者の数=' || hisyouniniraiCnt);
					--DBMS_OUTPUT.PUT_LINE('承認者の数　　　=' || tab_syouninsya_samesid.count);
					--承認のイベントを作成する
					--アサイン先予定を削除する
					delete from EIMASPLN where OID = p_objId and STTID = tab_eimst(i).sttype;
					FOR ii IN 0..tab_syouninsya_samesid.count-1 LOOP
						
						IF ii = hisyouniniraiCnt-1 THEN
						--全員承認で最後の承認の場合
							eventtypeid := sel_eventtypeid(tab_eimst(i).sttype, tab_eimst(i+1).sttype, CONS_BET_APPROVE);
							insert into EIMEV values(EIMID.nextval, p_objId, eventtypeid, tab_eimst(i).sid, tab_eimst(i+1).sid, tab_syouninsya_samesid(ii).cuser, tab_syouninsya_samesid(ii).cdate);
						ELSE
						--部分承認の場合
							eventtypeid := sel_eventtypeid(tab_eimst(i).sttype, tab_eimst(i).sttype, CONS_BET_APPROVE);
							insert into EIMEV values(EIMID.nextval, p_objId, eventtypeid, tab_eimst(i).sid, tab_eimst(i).sid, tab_syouninsya_samesid(ii).cuser, tab_syouninsya_samesid(ii).cdate);
						END IF;
						comment := tab_syouninsya_samesid(ii).wfcomment;
						select EIMID.currval into eventid from dual;
						IF comment is not null THEN
							insert into EIMEVSTR values(eventid, attTypeId_Comment, 0, comment);
						END IF;
						--アサイン先予定を作成する
						insert into EIMASPLN values(EIMID.nextval, p_objId, tab_eimst(i).sttype, CONS_ENTRYTYPE_USER, tab_syouninsya_samesid(ii).cuser);
						--アサイン先を作成する
						insert into EIMAS values(EIMID.nextval, tab_eimst(i).sid, CONS_ENTRYTYPE_USER, tab_syouninsya_samesid(ii).cuser, eventid);
					END LOOP;
					
					
				--■(全員承認)承認依頼取消：全員承認⇒全員承認、全員承認⇒一人承認、全員承認⇒編集中
				ELSIF tab_eimst(i).oldkind = 2 AND tab_eimst(i).step > tab_eimst(i+1).step AND sel_sendbackIdx(tab_eimst(i+1).sid) = NOT_FOUND AND (tab_eimst(i+1).oldkind = 1 OR tab_eimst(i+1).oldkind = 2 OR tab_eimst(i+1).oldkind = 3) THEN
					eventtypeid := sel_eventtypeid(tab_eimst(i).sttype, tab_eimst(i+1).sttype, CONS_BET_CANAPPREQ);
					--承認依頼取消のイベントを作成する
					insert into EIMEV values(EIMID.nextval, p_objId, eventtypeid, tab_eimst(i).sid, tab_eimst(i+1).sid, tab_eimst(i+1).cuser, tab_eimst(i+1).cdate);
					select EIMID.currval into eventid from dual;
					--被承認依頼者を取得する
					set_hiirai_samesid_array(tab_eimst(i).sid);
					--被承認依頼者の数だけアサイン先・アサイン先予定を作成する
					--アサイン先予定を削除する
					delete from EIMASPLN where OID = p_objId and STTID = tab_eimst(i).sttype;
					FOR ii IN 0..tab_hisyouninirai_samesid.count-1 LOOP
						--アサイン先予定を作成する
						insert into EIMASPLN values(EIMID.nextval, p_objId, tab_eimst(i).sttype, CONS_ENTRYTYPE_USER, tab_hisyouninirai_samesid(ii).entryid);
						--アサイン先を作成する
						insert into EIMAS values(EIMID.nextval, tab_eimst(i).sid, CONS_ENTRYTYPE_USER, tab_hisyouninirai_samesid(ii).entryid, 0);
						IF tab_hisyouninirai_samesid(ii).entryid = tab_eimst(i+1).cuser THEN
							select EIMID.currval into asignid from dual;
							--承認依頼先と承認依頼取消を行った人が同一の場合はEIMAS.evidを更新する
							update EIMAS set evid = eventid where asid = asignid;
						END IF;
					END LOOP;
					
				--■(全員承認)差戻し：全員承認⇒編集中
				ELSIF tab_eimst(i).oldkind = 2 AND tab_eimst(i).step > tab_eimst(i+1).step AND sel_sendbackIdx(tab_eimst(i+1).sid) <> NOT_FOUND AND (tab_eimst(i+1).oldkind = 1) THEN
					eventtypeid := sel_eventtypeid(tab_eimst(i).sttype, tab_eimst(i+1).sttype, CONS_BET_SENDBACK);
					--差戻しのイベントを作成する
					sendbackuser := tab_sendback(sel_sendbackIdx(tab_eimst(i+1).sid)).cuser;
					insert into EIMEV values(EIMID.nextval, p_objId, eventtypeid, tab_eimst(i).sid, tab_eimst(i+1).sid, sendbackuser, tab_eimst(i+1).cdate);
					select EIMID.currval into eventid from dual;
					comment := tab_sendback(sel_sendbackIdx(tab_eimst(i+1).sid)).wfcomment;
					IF comment is not null THEN
						select EIMID.currval into eventid from dual;
						insert into EIMEVSTR values(eventid, attTypeId_Comment, 0, comment);
					END IF;
					--被承認依頼者を取得する
					set_hiirai_samesid_array(tab_eimst(i).sid);
					--被承認依頼者の数だけアサイン先・アサイン先予定を作成する
					--アサイン先予定を削除する
					delete from EIMASPLN where OID = p_objId and STTID = tab_eimst(i).sttype;
					FOR ii IN 0..tab_hisyouninirai_samesid.count-1 LOOP
						--アサイン先予定を作成する
						insert into EIMASPLN values(EIMID.nextval, p_objId, tab_eimst(i).sttype, CONS_ENTRYTYPE_USER, tab_hisyouninirai_samesid(ii).entryid);
						--アサイン先を作成する
						insert into EIMAS values(EIMID.nextval, tab_eimst(i).sid, CONS_ENTRYTYPE_USER, tab_hisyouninirai_samesid(ii).entryid, 0);
						IF tab_hisyouninirai_samesid(ii).entryid = sendbackuser THEN
							select EIMID.currval into asignid from dual;
							--承認依頼先と承認依頼取消を行った人が同一の場合はEIMAS.evidを更新する
							update EIMAS set evid = eventid where asid = asignid;
						END IF;
					END LOOP;
					
					
				--■(公開処理中)公開：公開処理中⇒公開済
				ELSIF tab_eimst(i).oldkind = 4 AND tab_eimst(i).step < tab_eimst(i+1).step AND (tab_eimst(i+1).oldkind = 5) THEN
					eventtypeid := sel_eventtypeid(tab_eimst(i).sttype, tab_eimst(i+1).sttype, CONS_BET_PUBLIC);
					--公開のイベントを作成する
					insert into EIMEV values(EIMID.nextval, p_objId, eventtypeid, tab_eimst(i).sid, tab_eimst(i+1).sid, tab_eimst(i+1).cuser, tab_eimst(i+1).cdate);
					--アサイン先・アサイン先予定は作成しない
				END IF;
			
			ELSE
			--現在のステータスの場合。ステータスが公開済み、またはステータスが途中のもの
				IF tab_eimst(i).oldkind = 2 THEN
				--■現在のステータスが全員承認の場合
					--DBMS_OUTPUT.PUT_LINE('現在のステータスが全員承認の場合');
					eventtypeid := sel_eventtypeid(tab_eimst(i).sttype, tab_eimst(i).sttype, CONS_BET_APPROVE);
					--被承認依頼者の数を取得する
					hisyouniniraiCnt := get_hisyouniniraiCnt(tab_eimst(i).sid);
					--承認者を取得する
					set_syouninsya_samesid_array(tab_eimst(i).sid);
					tab_event.delete;
					eventCnt := 0;
					--承認のイベントを作成する
					FOR ii IN 0..tab_syouninsya_samesid.count-1 LOOP
						IF ii < hisyouniniraiCnt-1 THEN
						--部分承認の場合
							eventtypeid := sel_eventtypeid(tab_eimst(i).sttype, tab_eimst(i).sttype, CONS_BET_APPROVE);
							insert into EIMEV values(EIMID.nextval, p_objId, eventtypeid, tab_eimst(i).sid, tab_eimst(i).sid, tab_syouninsya_samesid(ii).cuser, tab_syouninsya_samesid(ii).cdate);
							select EIMID.currval into eventid from dual;
							tab_event(eventCnt).evid := eventid;
							tab_event(eventCnt).cuser := tab_syouninsya_samesid(ii).cuser;
							eventCnt := eventCnt + 1;
						END IF;
						comment := tab_syouninsya_samesid(ii).wfcomment;
						IF comment is not null THEN
							select EIMID.currval into eventid from dual;
							insert into EIMEVSTR values(eventid, attTypeId_Comment, 0, comment);
						END IF;
					END LOOP;
					--被承認依頼者を取得する
					set_hiirai_samesid_array(tab_eimst(i).sid);
					--被承認依頼者の数だけアサイン先・アサイン先予定を作成する
					asIdx := 0;
					apIdx := 0;
					tab_as.delete;
					tab_aspln.delete;
					--アサイン先予定を削除する
					delete from EIMASPLN where OID = p_objId and STTID = tab_eimst(i).sttype;
					--アサイン先予定リレーションを削除する
					delete from EIMASPLNREL where apid in (select apid from EIMASPLN where sttid = tab_eimst(i).sttype and oid = p_objId);
					FOR ii IN 0..tab_hisyouninirai_samesid.count-1 LOOP
						--アサイン先予定を作成する
						insert into EIMASPLN values(EIMID.nextval, p_objId, tab_eimst(i).sttype, CONS_ENTRYTYPE_USER, tab_hisyouninirai_samesid(ii).entryid);
						select EIMID.currval into asignplnid from dual;
						tab_aspln(apIdx).apid := asignplnid;
						tab_aspln(apIdx).sttid := tab_eimst(i).sttype;
						tab_aspln(apIdx).entry := tab_hisyouninirai_samesid(ii).entryid;
						apIdx := apIdx + 1;
						--アサイン先を作成する
						insert into EIMAS values(EIMID.nextval, tab_eimst(i).sid, CONS_ENTRYTYPE_USER, tab_hisyouninirai_samesid(ii).entryid, 0);
						select EIMID.currval into asignid from dual;
						tab_as(asIdx).asid := asignid;
						tab_as(asIdx).sttid := tab_eimst(i).sttype;
						tab_as(asIdx).entry := tab_hisyouninirai_samesid(ii).entryid;
						asIdx := asIdx + 1;
						--承認者の数だけループしてアサイン先の内、承認済みのものはEIMAS.evidを更新する
						FOR iii IN 0..tab_event.count-1 LOOP
							IF tab_event(iii).cuser = tab_hisyouninirai_samesid(ii).entryid THEN
								--被承認依頼者と承認者が同じ場合はEIMAS.evidを更新する
								update EIMAS set evid = tab_event(iii).evid where asid = asignid;
								EXIT;
							END IF;
						END LOOP;
					END LOOP;
					
					--アサイン先予定リレーションを作成する
					IF tab_aspln.count > 0 THEN
						cre_eimasplnrel();
					END IF;
					--アサイン先リレーションを作成する
					IF tab_as.count > 0 THEN
						cre_eimasrel();
					END IF;
					
					--再承認依頼先がある場合、次のステータスタイプのアサイン先予定を追加する
					--次のステータスタイプが一人承認の場合は再承認依頼先のエントリーをユーザに展開してアサイン先予定を作成する
					apIdx := 0;
					tab_aspln.delete;
					set_saiiraisaki_samesid_array(tab_eimst(i).sid);
					nextstatustypeid := 0;
					nextstatustypeid := get_nextstatustypeid(tab_eimst(i).sttype);
					--ユーザID格納用VARRAYの初期化
					userIdx := 1;
					userCnt := 0;
					userid_array.delete;
					
					--アサイン先予定を削除する
					delete from EIMASPLN where OID = p_objId and STTID = nextstatustypeid;
					--アサイン先予定リレーションを削除する
					delete from EIMASPLNREL where apid in (select apid from EIMASPLN where sttid = tab_eimst(i).sttype and oid = p_objId);
					
					--■次のステータスタイプが全員承認の場合
					--再承認依頼先はユーザ単位なのでそのままアサイン先予定を作成する
					IF get_kind(nextstatustypeid) = 2 THEN
						FOR ii IN 0..tab_saiiraisaki_samesid.count-1 LOOP
							--次のステータスタイプのアサイン先予定を作成する
							insert into EIMASPLN values(EIMID.nextval, p_objId, nextstatustypeid, CONS_ENTRYTYPE_USER, tab_saiiraisaki_samesid(ii).entryid);
							select EIMID.currval into asignplnid from dual;
							tab_aspln(apIdx).apid := asignplnid;
							tab_aspln(apIdx).sttid := nextstatustypeid;
							tab_aspln(apIdx).entry := tab_saiiraisaki_samesid(ii).entryid;
							apIdx := apIdx + 1;
						END LOOP;
					ELSIF get_kind(nextstatustypeid) = 3 THEN
					--■次のステータスタイプが一人承認の場合
					--再承認依頼先はユーザ以外(グループ・ロール・複合グループ)のエントリーの可能性もあるのでユーザに展開してアサイン先予定を作成する
						
						FOR ii IN 0..tab_saiiraisaki_samesid.count-1 LOOP
							IF tab_saiiraisaki_samesid(ii).entrytype = CONS_ENTRYTYPE_USER THEN
								userid_array.extend;
								userid_array(userIdx) := tab_saiiraisaki_samesid(ii).entryid;
								userIdx := userIdx + 1;
							ELSIF tab_saiiraisaki_samesid(ii).entrytype = CONS_ENTRYTYPE_GROUP THEN
								FOR r_groupuser IN c_recurgroupuser(tab_saiiraisaki_samesid(ii).entryid) LOOP
									userid_array.extend;
									userid_array(userIdx) := r_groupuser.userid;
									userIdx := userIdx + 1;
								END LOOP;
							ELSIF tab_saiiraisaki_samesid(ii).entrytype = CONS_ENTRYTYPE_ROLE THEN
								FOR r_roleuser IN c_roleuser(tab_saiiraisaki_samesid(ii).entryid) LOOP
									userid_array.extend;
									userid_array(userIdx) := r_roleuser.userid;
									userIdx := userIdx + 1;
								END LOOP;
							ELSIF tab_saiiraisaki_samesid(ii).entrytype = CONS_ENTRYTYPE_CMP THEN
								FOR r_cmpuser IN c_cmpuser(tab_saiiraisaki_samesid(ii).entryid) LOOP
									userid_array.extend;
									userid_array(userIdx) := r_cmpuser.userid;
									userIdx := userIdx + 1;
								END LOOP;
							END IF;
						END LOOP;
						
						--取得したユーザの重複を除くものでアサイン先予定を作成する
						FOR r_distinctuser IN c_distinctuser(userid_array) LOOP
							--アサイン先予定を作成する
							insert into EIMASPLN values(EIMID.nextval, p_objId, nextstatustypeid, CONS_ENTRYTYPE_USER, r_distinctuser.userid);
							select EIMID.currval into asignplnid from dual;
							tab_aspln(apIdx).apid := asignplnid;
							tab_aspln(apIdx).sttid := nextstatustypeid;
							tab_aspln(apIdx).entry := r_distinctuser.userid;
							apIdx := apIdx + 1;
						END LOOP;
						
					END IF;
					
					--アサイン先予定リレーションを作成する
					IF tab_aspln.count > 0 THEN
						cre_eimasplnrel();
					END IF;
					
				ELSIF tab_eimst(i).oldkind = 3 THEN
				--■現在のステータスが一人承認の場合
					--DBMS_OUTPUT.PUT_LINE('現在のステータスが一人承認の場合');
					--ユーザID格納用VARRAYの初期化
					userIdx := 1;
					userCnt := 0;
					userid_array.delete;
					--userid_array.extend(10000);
					--イベントは作成しない
					--承認依頼先がグループ・ロール・複合グループの場合はユーザに展開する。ただし、グループは子グループも再帰的に辿る。重複ユーザは削除する。
					
					--被承認依頼者を取得する
					set_hiirai_samesid_array(tab_eimst(i).sid);
					FOR ii IN 0..tab_hisyouninirai_samesid.count-1 LOOP
						IF tab_hisyouninirai_samesid(ii).entrytype = CONS_ENTRYTYPE_USER THEN
							userid_array.extend;
							userid_array(userIdx) := tab_hisyouninirai_samesid(ii).entryid;
							userIdx := userIdx + 1;
						ELSIF tab_hisyouninirai_samesid(ii).entrytype = CONS_ENTRYTYPE_GROUP THEN
							FOR r_groupuser IN c_recurgroupuser(tab_hisyouninirai_samesid(ii).entryid) LOOP
								userid_array.extend;
								userid_array(userIdx) := r_groupuser.userid;
								userIdx := userIdx + 1;
							END LOOP;
						ELSIF tab_hisyouninirai_samesid(ii).entrytype = CONS_ENTRYTYPE_ROLE THEN
							FOR r_roleuser IN c_roleuser(tab_hisyouninirai_samesid(ii).entryid) LOOP
								userid_array.extend;
								userid_array(userIdx) := r_roleuser.userid;
								userIdx := userIdx + 1;
							END LOOP;
						ELSIF tab_hisyouninirai_samesid(ii).entrytype = CONS_ENTRYTYPE_CMP THEN
							FOR r_cmpuser IN c_cmpuser(tab_hisyouninirai_samesid(ii).entryid) LOOP
								userid_array.extend;
								userid_array(userIdx) := r_cmpuser.userid;
								userIdx := userIdx + 1;
							END LOOP;
						END IF;
					END LOOP;
					
					--取得したユーザの重複を除くものでアサイン先・アサイン先予定を作成する
					asIdx := 0;
					apIdx := 0;
					tab_as.delete;
					tab_aspln.delete;
					--アサイン先予定を削除する
					delete from EIMASPLN where OID = p_objId and STTID = tab_eimst(i).sttype;
					--アサイン先予定リレーションを削除する
					delete from EIMASPLNREL where apid in (select apid from EIMASPLN where sttid = tab_eimst(i).sttype and oid = p_objId);
					FOR r_distinctuser IN c_distinctuser(userid_array) LOOP
						--アサイン先予定を作成する
						insert into EIMASPLN values(EIMID.nextval, p_objId, tab_eimst(i).sttype, CONS_ENTRYTYPE_USER, r_distinctuser.userid);
						select EIMID.currval into asignplnid from dual;
						tab_aspln(apIdx).apid := asignplnid;
						tab_aspln(apIdx).sttid := tab_eimst(i).sttype;
						tab_aspln(apIdx).entry := r_distinctuser.userid;
						apIdx := apIdx + 1;
						--アサイン先を作成する
						insert into EIMAS values(EIMID.nextval, tab_eimst(i).sid, CONS_ENTRYTYPE_USER, r_distinctuser.userid, 0);
						select EIMID.currval into asignid from dual;
						tab_as(asIdx).asid := asignid;
						tab_as(asIdx).sttid := tab_eimst(i).sttype;
						tab_as(asIdx).entry := r_distinctuser.userid;
						asIdx := asIdx + 1;
					END LOOP;
					
					--アサイン先予定リレーションを作成する
					IF tab_aspln.count > 0 THEN
						cre_eimasplnrel();
					END IF;
					--アサイン先リレーションを作成する
					IF tab_as.count > 0 THEN
						cre_eimasrel();
					END IF;
					
					--DBMS_OUTPUT.PUT_LINE('userIdx=' || userid_array.count);
					
					
				END IF;
			
			END IF;
			
		END LOOP;

	END;
	
	
BEGIN
	--**************************************************************************
	--メイン処理
	--**************************************************************************
	
	--前処理
	select id into objTypeId_MailNotification		from eimobjtype where name = 'メール通知';
	select id into attTypeId_Reply					from eimattr where name = '受信確認';
	select id into attTypeId_NotificationOfTiming	from eimattr where name = '公開通知タイミング';
	select id into attTypeId_ApprovalReqTiming		from eimattr where name = '承認依頼通知タイミング';
	select id into attTypeId_NotificationOfDest		from eimattr where name = '公開通知送信先';
	select id into attTypeId_Comment				from eimattr where name = 'コメント';
	
	userid_array := EIMARRAY();
	
	--START 移行対象のオブジェクト
	FOR r_target_obj IN c_target_obj LOOP
		--DBMS_OUTPUT.PUT_LINE('オブジェクトID=' || r_target_obj.id || ':ワークフローID=' || r_target_obj.workflow);
		
		BEGIN
			--引数のワークフローのEIMSTTYPEを配列に格納する
			idx := 0;
			cntEdit := 0;
			
			FOR r_eimsttype IN c_eimsttype(r_target_obj.workflow) LOOP
				tab_eimsttype(idx) := r_eimsttype;
				idx := idx + 1;
			END LOOP;
			
			--EIMSTの値を配列に全て格納する
			idx := 0;
			FOR r_eimst IN c_eimst(r_target_obj.id, r_target_obj.workflow) LOOP
				tab_eimst(idx) := r_eimst;
				idx := idx + 1;
			END LOOP;
			
			--差戻しで遷移した不必要なステータスを削除する
			del_senbackstatus(r_target_obj.id);
			
			--DEBUG
			/*
			FOR i IN 0..tab_eimst_del.count-1 LOOP
				DBMS_OUTPUT.PUT_LINE('削除対象のSID=' || tab_eimst_del(i).sid);
			END LOOP;
			*/
			
			--現在のステータスが編集中かどうかを判定する
			--編集中の場合は1が結果として返却される
			select
				count(*) as cnt into cntEdit
			from
			(
				select
					 X.id
					,(select STT.kind as kind from EIMSTTYPE STT where STT.id = (select ST.type from EIMST ST where ST.oid = X.id and ST.sid = X.status)) as kind
				from EIMOBJ X
				where
				X.id = r_target_obj.id
			) OBJST
			where
			OBJST.kind = -13001;
			
			--現在のステータスが編集中以外の場合、メール通知オブジェクトを作成する
			IF cntEdit <> 1 THEN
				--メール通知オブジェクトを作成する
				cre_mailnotification(r_target_obj.id);
			END IF;
			
			--EIMSTの値を配列に全て格納する(再取得)
			tab_eimst.delete;
			idx := 0;
			FOR r_eimst IN c_eimst(r_target_obj.id, r_target_obj.workflow) LOOP
				tab_eimst(idx) := r_eimst;
				idx := idx + 1;
			END LOOP;
			
			--承認関連のオブジェクトを取得する
			sel_wf_rireki(r_target_obj.id);
			
			--イベントを作成する
			cre_event(r_target_obj.id);
			
			--コミット
			update IKOU_TARGET_OBJ set ikou_flg = 1 where id = r_target_obj.id;
			commit;
			
		EXCEPTION
			WHEN OTHERS THEN
				wk_errmsg := SQLERRM;
				wk_errno := SQLCODE;
				wk_errmsgno := wk_errmsg || ':' || wk_errno;
				--DBMS_OUTPUT.PUT_LINE(wk_errmsgno);
				--ロールバック
				rollback;
				update IKOU_TARGET_OBJ set ikou_flg = -1, error = wk_errmsgno where id = r_target_obj.id;
				commit;
		END;
		
		--初期化処理
		tab_eimsttype.delete;
		tab_eimst_del.delete;
		tab_eimst.delete;
		idx := 0;
		delStatusCnt := 0;
		rcode := 0;
		
	END LOOP;
	--END   移行対象のオブジェクト
	
	commit;
	
END;
/
