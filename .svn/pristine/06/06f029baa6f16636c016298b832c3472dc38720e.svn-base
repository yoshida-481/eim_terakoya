<%@page import="jp.co.ctc_g.eim.framework2.business.domain.search.internal.SearchCondition"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.search.SearchOperatorEnum"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.service.ObjectService"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.criteria.ObjectTypeCriteria"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.search.SearchLimitCountCondition"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>

<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.admin.business.domain.NamespaceDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import ="jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService"%>
<%@ page import ="jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain"%>
<%@ page import ="jp.co.ctc_g.eim.framework2.business.service.SequenceService"%>
<%@ page import ="jp.co.ctc_g.eim.framework2.business.domain.entity.SequenceDomain"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	//Parameter
	String prmObjTypeId = request.getParameter("objTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"objTypeId=" + prmObjTypeId
			};

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

		// システム管理種別を取得
		String adminAppId = (String)session.getAttribute("ADMIN_APP_ID");

		/*
		 * Get Object Type
		 */
		 EIMObjectType objType = null;

		// 付加文字列
		String formatString = "";
		String publicClassName = "";

		// 番号自動生成
		boolean isNumberAutoCreate = false;

		// 汎用の場合
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {

			objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId));

		// 帳票管理 or ドキュメント管理の場合
		} else {

			// オブジェクトタイプを取得
			ObjectTypeService objectTypeService =
				(ObjectTypeService)ApplicationContextLoader.getApplicationContext().getBean("objectTypeService2");
			ObjectTypeDomain objectTypeDomain =
				objectTypeService.getById(Long.parseLong(prmObjTypeId));

			if (objectTypeDomain != null) {

				// 後続の処理をV4系に合わせるためコンバートをかける
				objType = ConvertUtils.toEIMObjectType(objectTypeDomain);

				if (objectTypeDomain.getSequence() != null) {
					isNumberAutoCreate = true;

					// 連続データの付加文字列を取得
					if (objectTypeDomain.getSequence() != null
							&& objectTypeDomain.getSequence().getFormatString() != null
							&& objectTypeDomain.getSequence().getFormatString().length() > 0) {

							formatString = objectTypeDomain.getSequence().getFormatString();
					}

					//シーケンスを共有しているクラスを取得
					//
					AttributeTypeService attributeTypeService = (AttributeTypeService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeService2");
					//該当クラスのシーケンスIDを取得
					long sequenceId = objectTypeDomain.getObjectTypeObject().getAttribute(EIMConfig.get("ATTRIBUTE_TYPE_NAME_SEQUENCE_ID")).getLong();
					//オブジェクトタイプObjectTypeを取得
					ObjectTypeDomain objTypeDomain = objectTypeService.getByDefinitionName(EIMConfig.get("OBJECT_TYPE_NAME_OBJECTTYPE"));
					//連続データ属性タイプを取得
					AttributeTypeDomain attributeTypeDomain = attributeTypeService.getByDefinitionName(EIMConfig.get("ATTRIBUTE_TYPE_NAME_SEQUENCE_ID"));

					//シーケンスを共有しているオブジェクトタイプObjectの検索条件を生成
					SearchSelectObject  searchSelectObject = new SearchSelectObject();
					SearchSelectObject.SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
					searchSelectObject.setCondition(helper.group(helper.opAnd()));
					//objTypeを指定
					searchSelectObject.getCondition().addCondition(helper.group(helper.opAnd()).addCondition(helper.eq(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, objTypeDomain.getId()) ) );
					//連続データ属性IDを指定
					searchSelectObject.getCondition().addCondition(helper.group(helper.opAnd()).addCondition(helper.eq(helper.opAnd(), attributeTypeDomain, sequenceId)));

					//検索上限設定
					SearchLimitCountCondition limitCond = new SearchLimitCountCondition(-1, true);
					ObjectService objectService = (ObjectService)ApplicationContextLoader.getApplicationContext().getBean("objectService2");
					//object検索
					List<ObjectDomain> objectDomainList = objectService.getList(searchSelectObject,limitCond);

					//該当クラスのオブジェクトタイプObjectIdを取得
					long targetObjectTypeObjId = objectTypeDomain.getObjectTypeObject().getId();
					Iterator ite = objectDomainList.iterator();
					MultipleCriteria<Long> publicClassObjTypeIdList = new MultipleCriteria<Long>();
					while(ite.hasNext())
					{
						ObjectDomain resultObjectDomain = (ObjectDomain)ite.next();
						if(targetObjectTypeObjId != resultObjectDomain.getId()){
							publicClassObjTypeIdList.add(Long.parseLong(resultObjectDomain.getName()));
						}
					}

					ObjectTypeCriteria objectTypeCriteria = new ObjectTypeCriteria();
					objectTypeCriteria.setIds(publicClassObjTypeIdList);
					List<ObjectTypeDomain> objectTypeDomainList = objectTypeService.getList(objectTypeCriteria);

					ite = objectTypeDomainList.iterator();
					int i=0;
					while(ite.hasNext())
					{
						ObjectTypeDomain publicClassObjType = (ObjectTypeDomain)ite.next();
						if(i == 0)
						{
							publicClassName = NamespaceUtil.getDefNamenWhichExceptedNamespace(publicClassObjType.getDefinitionName());
						}else{
							publicClassName += "," + NamespaceUtil.getDefNamenWhichExceptedNamespace(publicClassObjType.getDefinitionName());
						}
						i++;
					}

				}

			}
		}

		if(objType == null){
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		// 表示する更新対象オブジェクト定義名称
		String targetDispName = "";

		// 汎用 or 帳票管理 or タスク管理の場合
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_GENERAL)
				|| adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)
				|| adminAppId.equals(AppConstant.ADMIN_APP_ID_TASK)) {

			// 更新対象オブジェクトの定義名称をネームスペースを除いた名称で表示
			targetDispName = StringUtils.xmlEncode(NamespaceUtil.getDefNamenWhichExceptedNamespace(objType.getDefName()));
		}

		//Other Object Type
		List otherList = ObjectUtils.getOtherObjectTypeNameList(sess, objType.getId());

		//XML
		out.println("<objType");
			out.println(" objTypeId=\"" + objType.getId() + "\"");

			// 定義名称からネームスペースを取得
			out.println(" namespace=\"" + StringUtils.xmlEncode(NamespaceUtil.getNamespaceByDefName(objType.getDefName())) + "\"");
			// 定義名称からネームスペースを除いた名称を取得
			out.println(" defNameWhichExceptedNamespace=\"" + targetDispName + "\"");
			// 連続データの付加文字列を返却
			out.println(" formatString=\"" + StringUtils.xmlEncode(formatString) + "\"");

			if(otherList != null) {

				out.println(" " + LanguageFieldUtil.PARAM_OTHRE_CNT + "=\"" + otherList.size() + "\"");

			} if(objType.getParent() != null) {

				EIMObjectType parentObjType = null;
				parentObjType = objType.getParent();
				// 親オブジェクト表示名称
				String parentDispName = null;

				// ドキュメント管理の場合
				if (adminAppId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT)) {

					if (parentObjType.getDefName().equals("ドキュメント")) {
						// 一般ドキュメント
						parentDispName = EIMResource.getMessage(sess, "EIM.OBJECTTYPE.GENERAL");

					} else if (parentObjType.getDefName().equals("フォルダ")) {
						// 一般フォルダ
						parentDispName = EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.FOLDER.GENERAL");

					} else if (parentObjType.getDefName().equals("タグ")) {
						// 一般タグ
						parentDispName = EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.TAG.GENERAL");

					} else {

						// 親オブジェクトの定義名称をネームスペースを除いた名称で表示
						parentDispName = NamespaceUtil.getDefNamenWhichExceptedNamespace(parentObjType.getDefName());
					}

					//番号自動生成
					String numberAutoCreate = Boolean.valueOf(isNumberAutoCreate).toString();
					out.println(" numberAutoCreate=\"" + StringUtils.xmlEncode(numberAutoCreate) + "\"");
					//シーケンスを共有しているクラス名を表示
					out.println(" publicClassName=\"" + StringUtils.xmlEncode(publicClassName) + "\"");

					EIMObjectType rootObjectType = null;
					for (EIMObjectType parentObjectType = objType; parentObjectType != null; parentObjectType = parentObjectType.getParent()) {
						if (parentObjectType != null) {
							rootObjectType = parentObjectType;
						}
					}
					out.println(" rootObjTypeDefName=\"" + StringUtils.xmlEncode(rootObjectType.getDefName()) + "\"");

				// 汎用管理の場合
				} else if (adminAppId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {

					// 親オブジェクトの定義名称をネームスペース付で表示
					parentDispName = NamespaceUtil.getDefNameWithNamespaceParentheses(
							NamespaceUtil.getDefNamenWhichExceptedNamespace(parentObjType.getDefName()),
							parentObjType.getDefName());

				// 帳票管理の場合
				} else if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)) {

					// 親オブジェクトの定義名称をネームスペースを除いた名称で表示
					parentDispName = NamespaceUtil.getDefNamenWhichExceptedNamespace(parentObjType.getDefName());
					//シーケンスを共有しているクラス名を表示
					out.println(" publicClassName=\"" + StringUtils.xmlEncode(publicClassName) + "\"");

				// タスク管理の場合
				} else if (adminAppId.equals(AppConstant.ADMIN_APP_ID_TASK)) {

					// 親オブジェクトの定義名称をネームスペースを除いた名称で表示
					parentDispName = NamespaceUtil.getDefNamenWhichExceptedNamespace(parentObjType.getDefName());
				}

				out.println(" parentObjTypeId=\"" + objType.getParent().getId() + "\"");
				out.println(" parentObjTypeName=\"" + StringUtils.xmlEncode(parentDispName) + "\"");
			}
		out.println(">");
		for(int i=0;i<otherList.size();i++){
			EIMOtherName eimOtherName = (EIMOtherName)otherList.get(i);
			out.println("<lang");
				out.println(" otherLId=\"" + eimOtherName.getLangId() + "\"");
				out.println(" otherName=\"" + StringUtils.xmlEncode(eimOtherName.getName()) + "\"");
			out.println(">");
			out.println("</lang>");
		}

		out.println("</objType>");

	}
	catch(eim.bo.EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
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
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime2.getMessage(), paramId), eime2);
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
