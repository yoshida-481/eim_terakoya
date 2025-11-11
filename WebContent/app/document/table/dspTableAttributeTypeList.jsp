<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "common.bo.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	EIMSession sess = null;
	EIMUser user = null;

	//Parameter
	String prmTableId = request.getParameter("tableId");
	String prmTableDefName = request.getParameter("tableDefName");
	if(prmTableDefName != null){
		prmTableDefName = new String(prmTableDefName.getBytes("ISO_8859_1"), "UTF-8");
	}
	
	//Message
	String message = null;
	Object[] paramId = {
			"tableId=" + prmTableId,
			"tableDefName=" + prmTableDefName
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
		user = (EIMUser)sess.getAttribute("USER");

		EIMTable table = null;
		List attTypeList = null;
		
		if(prmTableId != null && prmTableId.length() > 0){
			// EIMテーブル選択時
			//Table
			table = TableUtils.getTableById(sess, Long.parseLong(prmTableId));
			
			//不正アクセスのチェック
			int count = 0;
			List tableList = TableUtils.getTableListByUser(sess, user);
			
			for(int i = 0; i < tableList.size(); i++)
			{
				//Table
				EIMTable table1 = (EIMTable)tableList.get(i);
				if(table1.getId() == table.getId()){
					count++;
					break;
				}
			}
			
			if(count == 0){
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ILLEGAL.REQUEST");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				log.warn(AppMessageUtils.makeLogMessage(message));
				return;
			}
			
			//Attribute Type List
			attTypeList = table.getAttributeList();
		}else if(prmTableDefName != null && prmTableDefName.length() > 0){
			// カスタマイズデフォルトテーブル選択時
			attTypeList = CustomDefaultTableUtils.getSelectedTableAttributeTypeListByDefName(sess, prmTableDefName);
		}
		
		// 属性マスタの有無を取得する
		HashSet masterExistSet = AttributeMasterUtil.getMasterExistSetByAttTypeIds(sess, attTypeList);

		//Root Node
		out.println("<attTypeList tableId=\"" + prmTableId + "\" tableDefName=\"" + prmTableDefName + "\">");
		//文書IDで検索可否フラグ
		boolean docSeachIndexFlag = Boolean.parseBoolean(EIMConfig.get("DOCUMENT_SEARCHINDEX_FLAG"));
		//自動採番機能の有効無効フラグ
		boolean isAutomaticNumbering = false;
		String automaticNumbering = EIMConfig.getValue("ENABLE_AUTOMATIC_NUMBERING");
		if(automaticNumbering.toUpperCase().equals("ON")){
			isAutomaticNumbering = true;
		}
		
		// NULLとなることがない属性リストを取得する
		String[] notNullAttributes = AppConstant.NOT_NULL_ATTRIBUTE_DEFNAME;
	
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

			//設定ファイルで文書IDでの検索を不可に設定している場合は
			if(attType.getDefaultName().equals(EIMConfig.get("ATTR_NAME_DOCUMENT_SEARCH_INDEX"))){
				//テーブルに表示を行わない。
				if(docSeachIndexFlag == false){
					continue;
				}
			}
			
			//設定ファイルで自動採番機能を不可に設定している場合は
			if(attType.getDefaultName().equals(EIMConfig.get("ATTR_NAME_DOCUMENT_NUMBER"))){
				//テーブルに表示を行わない。
				if(isAutomaticNumbering == false){
					continue;
				}
			}
			
			long attValTypeId = attType.getValueType().getId();

			// 入力規則
			boolean inputRule = false;
			long[] intValues = null;
			double[] doubleValues = null;
			String[] strValues = null;
			Date[] dateValues = null;
			String[] textValues = null;

			if (masterExistSet.contains(new Long(attType.getId()))) {
				// 属性タイプ値マスターの取得
				AttributeValueMaster attMaster =
					AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attType.getId());
				if(attMaster != null){

					inputRule = true;

					// 数値型
					if(attValTypeId == EIMValueType.INTEGER){
						try {
							intValues = TypeConvertUtils.convertToLongArray(attMaster.getInts());
						} catch(EIMException e){
							// 設定されていなければマスター定義無しとみなす
							intValues = null;
						}
					}
					// 文字列型
					else if(attValTypeId == EIMValueType.STRING){
						try {
							strValues = attMaster.getStrings();
						} catch(EIMException e){
							// 設定されていなければマスター定義無しとみなす
							strValues = null;
						}
					}
					// 日付型
					else if(attValTypeId == EIMValueType.DATE){
						try {
							dateValues = attMaster.getDates();
						} catch(EIMException e){
							// 設定されていなければマスター定義無しとみなす
							dateValues = null;
						}
					}
					//ダブル型
					else if(attValTypeId == EIMValueType.DOUBLE){
							try {
								doubleValues = attMaster.getDoubles();
							} catch(EIMException e){
								// 設定されていなければマスター定義無しとみなす
								doubleValues = null;
							}
					}
					// テキスト型
					else {
						try {
							textValues = attMaster.getTexts();
						} catch(EIMException e){
							// 設定されていなければマスター定義無しとみなす
							textValues = null;
						}
					}
				}
			}

			// NULLとなることがない属性リストに含まれる属性についてはnotNullをtrueとする
			String notNull = "false";
			if(Arrays.asList(notNullAttributes).contains(attType.getDefName())){
				notNull = "true";
			}
			
			//XML
			out.println("<attType");
				out.println(" attTypeId=\"" + attType.getId() + "\"");
				out.println(" attTypeName=\"" + StringUtils.xmlEncode(attType.getName()) + "\"");
				out.println(" attTypeDefName=\"" + StringUtils.xmlEncode(attType.getDefaultName()) + "\"");
				out.println(" attValTypeId= \""+String.valueOf(attType.getValueType().getId())+ "\"");
				out.println(" attIsMultiple= \""+String.valueOf(attType.isMultiple())+ "\"");
				out.println(" inputRule= \""+ Boolean.toString(inputRule) + "\"");
				out.println(" notNull= \""+ notNull + "\"");
			out.println(">");
			if(inputRule){

				// リスト1件目の空のカラム(未選択用)
				out.println("<attMaster value=\"\"/>");

				// 数値型
				if(attValTypeId == EIMValueType.INTEGER){
					if (intValues != null) {
						for(int j = 0; j < intValues.length ; j++){
							out.println("<attMaster value=\"" + String.valueOf(intValues[j]) + "\"/>");
						}
					}
				}
				// 文字列型
				else if(attValTypeId == EIMValueType.STRING){
					if (strValues != null) {
						for(int j = 0; j < strValues.length ; j++){
							out.println("<attMaster value=\"" + StringUtils.xmlEncode(strValues[j]) + "\"/>");
						}
					}
				}
				// 日付型
				else if(attValTypeId == EIMValueType.DATE){
					if (dateValues != null) {
						for(int j = 0; j < dateValues.length ; j++){
							String convDate = DateUtils.getDBTzToCLTzDate(sess, dateValues[j]);
							out.println("<attMaster value=\"" + convDate + "\"/>");
						}
					}
				}
				//ダブル型
				else if(attValTypeId == EIMValueType.DOUBLE){
					if (doubleValues != null) {
						for(int j = 0; j < doubleValues.length ; j++){
							out.println("<attMaster value=\"" + FormatUtil.getDoubleFormatedString(doubleValues[j]) + "\"/>");
						}
					}
				}
				// テキスト型
				else {
					if (textValues != null) {
						for(int j = 0; j < textValues.length ; j++){
							out.println("<attMaster value=\"" + StringUtils.xmlEncode(textValues[j]) + "\"/>");
						}
					}
				}
			}
			out.println("</attType>");
		}

		//End Root Node
		out.println("</attTypeList>");
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
