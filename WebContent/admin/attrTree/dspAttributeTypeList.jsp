<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%!
	/**
	 * ドキュメント系列タイプ/WS系列タイプに割り当てられている属性をリストで返す。<br>
	 * 管理用の属性とダミー属性は除外する
	 * @param sess セッション
	 * @return ドキュメント系列タイプ/WS系列タイプに割り当てられている属性のセット
	 * @throws Exception 予期せぬ例外
	 */
	private Set getDocumentAttributes(EIMSession sess) throws Exception {
		List objTypes = new ArrayList();

		// ドキュメントタイプの取得
		EIMObjectType docType = ObjectUtils.getObjectTypeByName(
				sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT")
				);
		objTypes.add(docType);
		// ドキュメントタイプの子オブジェクトタイプの取得
		ObjectUtils.getChildObjectTypeListRecurrently(sess, docType, objTypes);

		// ワークスペースのオブジェクトタイプの取得
		//   (フォルダタイプに継承する属性を取得するため)
		EIMObjectType workSpaceType = ObjectUtils.getObjectTypeByName(
				sess, EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE")
		);
		objTypes.add(workSpaceType);

		// フォルダタイプの取得
		EIMObjectType folderType = ObjectUtils.getObjectTypeByName(
				sess, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER")
		);
		objTypes.add(folderType);
		// フォルダタイプの子オブジェクトタイプの取得
		ObjectUtils.getChildObjectTypeListRecurrently(sess, folderType, objTypes);
		
		Set attrs = new TreeSet(
				new Comparator() {
					public int compare(Object o1, Object o2) {
						return new Long(((EIMAttributeType)o1).getId() - ((EIMAttributeType)o2).getId()).intValue();
					}
				}
			);
		for (Iterator i = objTypes.iterator(); i.hasNext(); ) {
			EIMObjectType objType = (EIMObjectType)i.next();
			attrs.addAll(
					ObjectAttributeUtils.getAttributeTypeListNoParent(sess, objType)
					);
		}
		
		//非表示属性の除外用リスト
		List excludeAttrNames = new ArrayList();
		excludeAttrNames.addAll(Arrays.asList(AppConstant.NONTABLE_ATTRIBUTE_DEFNAME));
		excludeAttrNames.addAll(Arrays.asList(AppConstant.NONATTRTREE_ATTRIBUTE_DEFNAME));

		for (Iterator i = attrs.iterator(); i.hasNext(); ) {
			EIMAttributeType attrType =(EIMAttributeType)i.next();
			//除外リストに入っているものは返却リストから削除する
			if (excludeAttrNames.contains(attrType.getDefName()))
			{
				i.remove();
				continue;
			}
			
			// オブジェクト型 or ユーザ型 or コード型は返却リストから削除する
			switch (attrType.getValueType().getId()) {
				// オブジェクト型
				case EIMValueType.OBJECT:
				// ユーザ型
				case EIMValueType.USER:
				// コード型
				case EIMValueType.CODE:
					i.remove();
					continue;
			}
		}

		return attrs;
	}
%>

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
	String prmAttTypeName = EIMUtils.getParameter(request, "attTypeName");

	//Message
	String message = null;
	Object[] paramId = {
			"attTypeName=" + prmAttTypeName
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_ATTRIUTE_TREE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		//Attribute Type List
		List attTypeList = AttributeUtils.getAttributeTypeListByOtherName(sess, prmAttTypeName);
		//開発課題289対応:ドキュメントで使用できる属性だけを返す
		attTypeList.retainAll(getDocumentAttributes(sess));

		//Root Node
		out.println("<attTypes>");

		// 入力規則の存在判定用HashSet
		HashSet masterExistSet = AttributeMasterUtil.getMasterExistSetByAttTypeIds(sess, attTypeList);
		
		for(int i = 0; i < attTypeList.size(); i++)
		{
			//Attribute Type
			EIMAttributeType attType = (EIMAttributeType)attTypeList.get(i);
			
			// 入力規則
			String inputRuleValue;
			if (masterExistSet.contains(new Long(attType.getId()))) {
				inputRuleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.INPUT.RULE.LIST");	// リスト定義
			} else {
				inputRuleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.INPUT.RULE.NONE");	// なし
			}
			
			// 複数値属性
			String isMultipleValue;
			if (attType.isMultiple()) {
				isMultipleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.MULTIPLE.MAINTENANCE");	// 保持する
			} else {
				isMultipleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.MULTIPLE.NONE");		// なし
			}
			
			//XML Out
			out.println("<attType");
				out.println(" attTypeId=\"" + attType.getId() + "\"");
				out.println(" attTypeName=\"" + StringUtils.xmlEncode(attType.getName()) + "\"");
				out.println(" valTypeName=\"" + StringUtils.xmlEncode(attType.getValueType().getName()) + "\"");
				out.println(" inputRuleValue=\"" + inputRuleValue + "\"");		// 入力規則
				out.println(" isMultipleValue=\"" + isMultipleValue + "\"");	// 複数値属性
				out.println(">");
			out.println("</attType>");
		}
		
		//End Root Node
		out.println("</attTypes>");
		
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
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
