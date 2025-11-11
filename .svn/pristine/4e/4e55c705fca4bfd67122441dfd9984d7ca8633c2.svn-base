<%@ page contentType = "text/xml; charset=UTF-8" %>

<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain"%>
<%@ page import ="jp.co.ctc_g.eim.framework2.business.domain.criteria.ObjectTypeCriteria"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	class RecurrentUtils
	{
		// オブジェクトタイプService
		ObjectTypeService objectTypeService = null;

	   /**
		* コンストラクタ
		*/
		public RecurrentUtils()
		{
			objectTypeService =
				(ObjectTypeService)ApplicationContextLoader.getApplicationContext().getBean("objectTypeService2");
		}

	   /**
	    * 再帰的にオブジェクトタイプツリーを表示する
	    *
	    * @param sess EIMSessionインスタンス
	    * @param adminAppId システム管理アプリケーション種別
	    * @param objType 親オブジェクトタイプ
	    * @param rootObjTypeName ルートのオブジェクトタイプデフォルト名称
	    * @param objTypeCriteria オブジェクトタイプ検索条件
	    * @param out
	    */
		public void write(EIMSession sess, String adminAppId, ObjectTypeDomain objTypeDomain,
				String rootObjTypeName, ObjectTypeCriteria objTypeCriteria, JspWriter out)
		throws Exception
		{
			// 親オブジェクトIDを設定
			objTypeCriteria.setParentId(new Long(objTypeDomain.getId()));

			// Child Object Type
			List<ObjectTypeDomain> childObjList = objectTypeService.getList(objTypeCriteria);

			for(int i = 0; i < childObjList.size(); i++)
			{
				//Object Type
				ObjectTypeDomain childObjType = childObjList.get(i);

				//XML
				out.println("<node");

					// 汎用システム管理の場合
					if (adminAppId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {

						// 定義名称をネームスペース付で表示
						out.println(" label=\"" + StringUtils.xmlEncode(
								NamespaceUtil.getDefNameWithNamespaceParentheses(
										childObjType.getName(), childObjType.getDefinitionName())) + "\"");

					} else {

						String dispName = "";

						// ドキュメント管理の場合
						if (adminAppId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT)) {

							// ログイン言語名称をクラス名称として表示
					 		dispName = childObjType.getName();

						// 帳票管理用 or タスク管理システム管理の場合
						} else {

							// ログイン言語名称をクラス名称として表示
					 		dispName = NamespaceUtil.getDefNamenWhichExceptedNamespace(childObjType.getName());
						}

						out.println(" label=\"" + StringUtils.xmlEncode(dispName) + "\"");
					}

					out.println(" objTypeId=\"" + childObjType.getId() + "\"");
					out.println(" objTypeName=\"" + StringUtils.xmlEncode(childObjType.getName()) + "\"");
					out.println(" objTypeDefName=\"" + StringUtils.xmlEncode(childObjType.getDefinitionName()) + "\"");
					out.println(" rootObjTypeDefName=\"" + StringUtils.xmlEncode(rootObjTypeName) + "\"");
				out.println(">");

				//Recurrenty
				write(sess, adminAppId, childObjType, rootObjTypeName, objTypeCriteria, out);

				out.println("</node>");
			}
		}
	}

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	//Parameter
	// [09/03/10 added by ik.]
	// 選択されているオブジェクトタイプID（システム管理（ドキュメント）のオブジェクトタイプ編集時のみ）
	String prmSelectedObjTypeId = EIMUtils.getParameter(request, "selectedObjTypeId");

	//Message
	String message = null;

	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		//User
		loginUser = (EIMUser)sess.getAttribute("USER");
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_DOCUMENTTYPE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		// システム管理アプリケーション種別を取得
		String adminAppId = (String)session.getAttribute("ADMIN_APP_ID");

		boolean isAdminAppDocument = adminAppId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT);
		boolean isAdminAppTask = adminAppId.equals(AppConstant.ADMIN_APP_ID_TASK);

		List<EIMObjectType> objTypeList;

		// ドキュメント管理の場合
		if (isAdminAppDocument) {

			objTypeList = new ArrayList<EIMObjectType>();
			// [09/03/10 added by ik.]
			// オブジェクトタイプ指定がある場合（＝システム管理（ドキュメント）でのオブジェクトタイプ編集）、
			// 対象オブジェクトタイプが属するオブジェクトタイプツリーのみにフィルタリングして表示。
			if (prmSelectedObjTypeId != null) {
				// 対象オブジェクトタイプが属するオブジェクトツリーのルートを取得
				EIMObjectType targetType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmSelectedObjTypeId));
				while (targetType.getParent() != null) {
					targetType = targetType.getParent();
					// フォルダの場合は、最上位が「ワークスペース」であるため、「フォルダ」までたどったら break
					if (targetType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"))) {
						break;
					}
				}
				objTypeList.add(targetType);
			} else {
				objTypeList.add(ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT")));	// ドキュメント
				objTypeList.add(ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER")));	// フォルダ
				objTypeList.add(ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_TAG")));	    // タグ
				objTypeList.add(ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_FORM_ATTACH_FILE")));	    // 文書添付ファイル
				objTypeList.add(ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_TEMP_ATTACH_FILE")));	    // 一時添付ファイル
			}

		} else {

			// 帳票管理の場合
			if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)) {

				objTypeList = new ArrayList<EIMObjectType>();

				EIMObjectType formObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_FORM_TYPE"));	                 //帳票
				if(formObjType != null) objTypeList.add(formObjType);
				EIMObjectType formAttachObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_FORM_ATTACH_FILE"));     //帳票添付ファイル
				if(formAttachObjType != null) objTypeList.add(formAttachObjType);
				EIMObjectType formTempAttachObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_TEMP_ATTACH_FILE")); //一時添付ファイル
				if(formTempAttachObjType != null) objTypeList.add(formTempAttachObjType);
			// タスク管理の場合
			} else if (adminAppId.equals(AppConstant.ADMIN_APP_ID_TASK)) {
				// 全て取得
				objTypeList = ObjectUtils.getObjectTypeListNoParent(sess);
				// ネームスペースで絞込み
				Iterator<EIMObjectType> it = objTypeList.iterator();
				while(it.hasNext()){
					EIMObjectType objType = it.next();

				   if(!objType.getDefName().startsWith(EIMConfig.get("APP_TASK_NAMESPACE_DEV"))) {
					   it.remove();
				   }
				}

				// ワークスペースを追加
				objTypeList.add(0, ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE")));			// ワークスペース
				// 文書（帳票）添付ファイルを追加
				objTypeList.add(ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_FORM_ATTACH_FILE")));	    // 文書添付ファイル
				// 一時添付ファイルを追加
				objTypeList.add(ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_TEMP_ATTACH_FILE")));	    // 一時添付ファイル

			// 汎用システム管理の場合
			} else {

				//全て取得
				//Root Object Type List
				objTypeList = ObjectUtils.getObjectTypeListNoParent(sess);
			}

			//以外は出力なし
		}

		//Recurrently
		RecurrentUtils ru = new RecurrentUtils();
		String label = null;

		// ネームスペースを取得
		String namespaceStr = (String)sess.getAttribute("ADMIN_NAMESPACE");

		ObjectTypeCriteria objTypeCriteria = new ObjectTypeCriteria();

		// ドキュメント管理 or 帳票管理 or タスク管理の場合
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT)
				|| adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)
				|| adminAppId.equals(AppConstant.ADMIN_APP_ID_TASK)) {

			// 検索条件にネームスペースを追加する
			String defSearchName = NamespaceUtil.concatenate(namespaceStr, null) + "*";
			objTypeCriteria.setDefinitionName(defSearchName);
		}

		if(EIMThreadContext.getTransactionContext() != null)
		{
			EIMThreadContext.removeTransactionContext();
		}
		// TransactionContextの作成、設定
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));

		//Root Node
		out.println("<nodes>");

		for(int i = 0; i < objTypeList.size(); i++)
		{
			//Object Type
			EIMObjectType objType = (EIMObjectType)objTypeList.get(i);

			if (isAdminAppDocument) {
				if (objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"))) {
					label = EIMResource.getMessage(sess, "EIM.OBJECTTYPE.GENERAL");					// 一般ドキュメント
				} else if (objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"))) {
					label = EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.FOLDER.GENERAL");	// 一般フォルダ
				} else if (objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_TAG"))) {
					label = EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.TAG.GENERAL");		// 一般タグ
				} else if (objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_FORM_ATTACH_FILE"))) {
					label = EIMResource.getMessage(sess, "EIM.OBJECTTYPE.DOCUMENT_ATTACH_FILE");	// 文書添付ファイル
				} else if (objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_TEMP_ATTACH_FILE"))) {
					label = EIMResource.getMessage(sess, "EIM.OBJECTTYPE.TEMP_ATTACH_FILE");		// 一時添付ファイル
				}
			} else {

				// 帳票管理 or タスク管理の場合
				if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM) || adminAppId.equals(AppConstant.ADMIN_APP_ID_TASK)) {

					// ログイン言語名称をクラス名称として表示
			 		label = NamespaceUtil.getDefNamenWhichExceptedNamespace(objType.getName());

					if (isAdminAppTask && objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_FORM_ATTACH_FILE"))) {
						label = EIMResource.getMessage(sess, "EIM.OBJECTTYPE.DOCUMENT_ATTACH_FILE");	// 文書添付ファイル
					}

				// 汎用システム管理の場合
				} else {

					// 定義名称をネームスペース付で表示
					label = NamespaceUtil.getDefNameWithNamespaceParentheses(objType.getName(), objType.getDefName());
				}
			}

			//ルートのオブジェクトタイプデフォルト名称
			String rootObjTypeName = StringUtils.xmlEncode(objType.getDefName());

			//XML
			out.println("<node");
				out.println(" label=\"" + StringUtils.xmlEncode(label) + "\"");
				out.println(" objTypeId=\"" + objType.getId() + "\"");
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(objType.getName()) + "\"");
				out.println(" objTypeDefName=\"" + StringUtils.xmlEncode(objType.getDefName()) + "\"");
				out.println(" rootObjTypeDefName=\"" + StringUtils.xmlEncode(rootObjTypeName) + "\"");

				// ドキュメント管理の場合
				if (isAdminAppDocument) {

					// 「一般ドキュメント」「一般フォルダ」「一般タグ」「文書添付ファイル」「一時添付ファイル」がルート
					out.println(" isRootType=\"true\"");

				// 汎用システム管理以外（帳票システム管理）の場合、ルート設定
				} else if (!adminAppId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {

						// ルート設定
						out.println(" isRootType=\"true\"");
				}

			out.println(">");

			ObjectTypeDomain rootObjTypeDomain = new ObjectTypeDomain();
			rootObjTypeDomain.setId(objType.getId());

			//Recurrently
			ru.write(sess, adminAppId, rootObjTypeDomain, rootObjTypeName, objTypeCriteria, out);

			out.println("</node>");
		}

		//End Root Node
		out.println("</nodes>");
	}
	catch(eim.bo.EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage()), eime);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	catch(jp.co.ctc_g.eim.framework2.common.exception.EIMException eime2)
	{
		out.clear();
		message = eime2.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime2.getMessage()), eime2);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	finally
	{
		try{
			if(EIMThreadContext.getTransactionContext() != null){
				EIMThreadContext.removeTransactionContext();
			}
			if(sess != null){
				sess.close();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>
