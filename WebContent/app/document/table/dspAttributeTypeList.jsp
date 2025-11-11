<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%!
	/**
	 * ドキュメント系列タイプ/WS系列タイプ/タグ系列タイプに割り当てられている属性をリストで返す
	 * @param sess セッション
	 * @return ドキュメント系列タイプ/WS系列タイプ/タグ系列タイプに割り当てられている属性のセット
	 * @throws Exception 予期せぬ例外
	 */
	private Set getDocumentFolderAttributes(EIMSession sess) throws Exception {
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
		
		// タグタイプの取得
		EIMObjectType tagType = ObjectUtils.getObjectTypeByName(
				sess, EIMConfig.get("OBJECT_TYPE_NAME_TAG")
		);
		objTypes.add(tagType);
		// タグタイプの子オブジェクトタイプの取得
		ObjectUtils.getChildObjectTypeListRecurrently(sess, tagType, objTypes);
		
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
		return attrs;
	}
%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	EIMSession sess = null;
	EIMUser user = null;
	
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
		user = (EIMUser)sess.getAttribute("USER");

		//Attribute Type List
		List attTypeList = AttributeUtils.getAttributeTypeListByOtherName(sess, prmAttTypeName);
		attTypeList.retainAll(getDocumentFolderAttributes(sess));
		
		// テーブルに非表示の属性は表示しない
		String[] nonTableAttributes = AppConstant.NONTABLE_ATTRIBUTE_DEFNAME;

		//Root Node
		out.println("<attTypes>");
		//文書IDで検索可否フラグ
		boolean docSeachIndexFlag = Boolean.parseBoolean(EIMConfig.get("DOCUMENT_SEARCHINDEX_FLAG"));
		
		ATTR_LIST_OUTPUT_LOOP:
		for(int i = 0; i < attTypeList.size(); i++)
		{
			//Attribute Type
			EIMAttributeType attType = (EIMAttributeType)attTypeList.get(i);

			// オブジェクト型、ユーザ型、コード型は、テーブル列に表示しない
			long valueTypeId = attType.getValueType().getId();
			if (valueTypeId == EIMValueType.OBJECT
					|| valueTypeId == EIMValueType.USER
					|| valueTypeId == EIMValueType.CODE)
			{
				continue;
			}

			// テーブルに非表示の属性は表示しない
			String defName = attType.getDefName();
			for (int j=0; j<nonTableAttributes.length; j++)
			{
				if(defName.equals(EIMConfig.get("ATTR_NAME_DOCUMENT_SEARCH_INDEX"))){
					
					//文書IDで検索不可を設定している場合
					if(docSeachIndexFlag == false){
						continue ATTR_LIST_OUTPUT_LOOP;
					}
				}
				if (defName.equals(nonTableAttributes[j]))
					continue ATTR_LIST_OUTPUT_LOOP;
			}
			//署名・暗号化オプションがOFFのときは「署名・暗号化バージョン」のデータを出力しない
			if(!OptionConfData.getInstance().SignAndEncrFlg){
				if(attType.getName().equals("署名・暗号化バージョン")){
					continue;
				}
			}
			
			//XML Out
			out.println("<attType");
				out.println(" attTypeId=\"" + attType.getId() + "\"");
				out.println(" attTypeName=\"" + StringUtils.xmlEncode(attType.getName()) + "\"");
				out.println(" valTypeName=\"" + StringUtils.xmlEncode(attType.getValueType().getName()) + "\"");
				out.println(" attValTypeId=\"" + attType.getValueType().getId() + "\"");
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
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
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
