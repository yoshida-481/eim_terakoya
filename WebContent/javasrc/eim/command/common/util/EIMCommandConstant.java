package eim.command.common.util;

import eim.util.EIMConfig;

public interface EIMCommandConstant {

	public static final String	LOGIN			= "login";
	public static final String	PWD				= "pwd";
	public static final String	LS				= "ls";
	public static final String	CD				= "cd";
	public static final String	RM				= "rm";
	public static final String	MV				= "mv";
	public static final String	CP				= "cp";
	public static final String	GET				= "get";
	public static final String	GET_BY_ID		= "getbyid";
	public static final String	PUT				= "put";
	public static final String	UPDATE_ATTR		= "updateattr";
	public static final String	FIND			= "find";
	public static final String	GET_ATTR		= "getattr";
	public static final String	CHECK_IN_BY_USER	= "checkinbyuser";
	public static final String	CHECK_OUT_BY_USER	= "checkoutbyuser";
	public static final String	MKDIR			= "mkdir";
	public static final String	QUIT			= "quit";
	public static final String	EXIT			= "exit";

	public static final String	ENC				= "enc";
	public static final String	VERIFY		= "verify";

	public static final String	SELECT		= "select";
	public static final String	CREATE		= "create";
	public static final String	UPDATE		= "update";
	public static final String	DELETE		= "delete";
	public static final String	CHECKIN		= "checkin";
	public static final String	CHECKOUT	= "checkout";

	public static final String	CMD				= "cmd";
	public static final String	USER			= "user";
	public static final String	PASS			= "pass";
	public static final String	PATH			= "path";
	public static final String	NAME			= "name";
	public static final String	TYPE			= "type";
	public static final String	OPTION		= "opt";
	public static final String	FORMAT		= "fmt";
	public static final String	ID				= "id";
	public static final String	FUNCTION	= "func";
	public static final String	OBJID			= "objId";
	public static final String	FILE			= "file";
	public static final String	PROCUSER		= "procUser";
	public static final String	ATTNAME			= "attName";
	public static final String	ATTVALUE		= "attValue";
	public static final String  UPLOAD_FILES     = "files";
	public static final String[] PARAMETERS = {
		CMD,
		USER,
		PASS,
		PATH,
		NAME,
		TYPE,
		OBJID,
		ATTNAME,
		ATTVALUE,
		OPTION,
		FORMAT,
		ID,
		FUNCTION
	};

	public static final String	INFO_RESULT_JSP	= "/WEB-INF/jsp/eim/command/EIMCommandObjectData.jsp";
	public static final String	INFO_RESULT_LIST_JSP = "/WEB-INF/jsp/eim/command/EIMCommandObjectDataList.jsp";
	public static final String	ERROR_RESULT_JSP	= "/WEB-INF/jsp/eim/command/EIMCommandError.jsp";

	/**
	 * アプリケーション種別ID：外部コマンド
	 */
	public static final String COMMAND = "3";

	/**
	 * 操作履歴用 操作種別
	 */
	public static final String GET_EXCOMMAND = "2056";
	public static final String GET_BY_ID_EXCOMMAND = "2057";
	public static final String PUT_EXCOMMAND = "2058";
	public static final String UPDATE_ATTRIBUTE_EXCOMMAND = "2059";
	public static final String LIST_EXCOMMAND = "2060";
	public static final String GET_ATTRIBUTE_EXCOMMAND = "2062";
	public static final String CHECK_OUT_BY_USER_EXCOMMAND = "2065";
	public static final String CHECK_IN_BY_USER_EXCOMMAND = "2066";
	public static final String CREATE_FOLDER = "2068";
	/**
	 * 操作履歴用 操作対象情報
	 */
	public static final String TARGET_TO_CREATE = "1";
	public static final String TARGET_TO_UPDATE = "3";
	public static final String TARGET_TO_DOWNLOAD = "25";
	public static final String TARGET_TO_LIST = "46";
	public static final String TARGET_TO_GET_OBJECT_INFO = "47";
	public static final String PROC_USER = "48";
	
	/**
	 * findコマンドのデフォルト返却項目
	 */
	public static final String[] FIND_COMMAND_DEFAULT_ATTRIBUTE_NAMES = {
									EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"),	// 署名・暗号化状態
									EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"),				// パス
									EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_VER"),		// 署名・暗号化バージョン番号
									EIMConfig.get("ATTR_NAME_DOCUMENT_SIZE"),				// ファイルサイズ
									EIMConfig.get("ATTR_NAME_DOCUMENT_DOCUMENT_LINK"),		// ドキュメントリンク
									EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK")		// リンク先
	};
	
	/**
	 * WF無しステータス「編集中」
	 */
	public static final int NOWF_EDIT = 0;

	/**
	 * WF無しステータス「改定中」
	 */
	public static final int NOWF_ON_REVISION = 1;

	/**
	 * WF無しステータス「公開済」
	 */
	public static final int NOWF_PUBLIC = 2;
	
	
	/**
	 * WF無しドキュメント「公開済」searchObjects用定数
	 */
	public static final String NOWF_PUBLIC_OBJSQL = "select eimobj.id from " +
													" eimobj,eimver, " +
												     " (select oob.id, vo.vid, oob.rev "+
												       " from "+
												         " eimobj oob, "+ 
												         " eimver vo, "+
												         " (select eimver.vid ,max(rev)-1 as rev "+
												           " from "+
												            "  eimobj iob, "+
												             " eimver "+
												            " where "+
												            "  eimver.oid = iob.id group by eimver.vid "+
												         " ) vv "+
												          " where "+
												           " oob.id = vo.oid and vv.vid = vo.vid "+
												           " and oob.rev = vv.rev "+
												           " and oob.status = 0 "+
												           " and oob.latest = 0 "+
												           " and oob.luser is null " +
												      " ) LB "+
													" where "+
														" eimobj.id = eimver.oid " +
														" and LB.rev + 1 = eimobj.rev " +
														" and eimobj.luser is null " +
														" and eimobj.status = 0 " +
														" and eimver.vid = LB.vid "
													+
													" union "+
													" select id "+
													  "  from eimobj "+
													   " where "+
													     " rev = 0 "+
													     " and latest = 1 "+
													     " and luser is null "+
													     " and status = 0 ";
	
	/**
	 * WF無しドキュメント「改定中」searchObjects用定数
	 */
	public static final String NOSTATAS_PUBLIC_EDIT_OBJSQL = " select eimobj.id as ID "+
    " from "+
    " eimobj, eimver, "+
    " (select oob.id as LID,vo.vid, oob.rev "+
    "  from "+
      "  eimobj oob, " +
       " eimver vo, " +
       " (select eimver.vid ,max(rev) as rev "+
        "  from "+
          "  eimobj iob, "+ 
           " eimver "+
         " where "+
         "   eimver.oid = iob.id group by eimver.vid "+
        " ) vv "+
         "where "+
         " oob.id = vo.oid and vv.vid = vo.vid "+
         " and oob.rev = vv.rev "+
         " and oob.status = 0 and oob.rev > 0 "+
    " ) LO "+
   " where "+
    "  eimobj.id = eimver.oid "+
     " and LO.rev - 1 = eimobj.rev "+
     " and eimver.vid = Lo.vid "+
     " and eimobj.luser > 0 ";
	

	
	/**
	 * Session属性：「DBサーバのオフセット・タイムゾーン」
	 */
	public static final String SESSION_ATTRIBUTE_DBTZ_OFFSET_FOR_EIMCOMMAND = "dbTzOffsetForEIMCommand";
	
	/**
	 * Session属性：「DBサーバ端末のタイムゾーンとGMT標準値の時差」
	 */
	public static final String SESSION_ATTRIBUTE_DBTZ_OFFSET = "dbTzOffset";
	
	/**
	 * Session属性：「クライアント端末のタイムゾーンとGMT標準値の時差」
	 */
	public static final String SESSION_ATTRIBUTE_CLIENTTZ_OFFSET = "clTzOffset";
	
	/**
	 * ソート用メソッド名：「最終更新日」
	 */
	public static final String SORT_METHOD_NAME_MDATE = "getModifyDateOfTarget";
	
	/**
	 * ソート用メソッド名：「名称」	
	 */
	public static final String SORT_METHOD_NAME_NAME = "getNameOfTarget";
	
	/**
	 * 検索用日時値
	 */
	public static final String FIND_COMMAND_DATE_FROM = " 00:00:00";
	public static final String FIND_COMMAND_DATE_TO = " 23:59:59";
	
	
}
