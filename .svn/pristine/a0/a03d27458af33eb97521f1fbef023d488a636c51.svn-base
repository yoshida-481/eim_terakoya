<%@page import="java.io.PrintWriter"%>
<%@page import="java.io.StringWriter"%>
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

	class RecurrentUtils
	{
		private EIMSession _sess;
		private AppObjectConditionHelper _helper;
		/** コンストラクタ */
		public RecurrentUtils(EIMSession sess) {
			_sess = sess;
			_helper = new AppObjectConditionHelper(sess);
		}
		
		/**
		 * 再帰的に属性ツリーを取得します。
		 *
		 * @param attrTree 属性ツリー
		 * @param prmAttrTreeValueList 属性ツリーの値リスト
		 * @param out 対象オブジェクト用のPrintWriter
		 * @param parentOut 対象オブジェクトの親用のPrintWriter
		 * @param searchLevel 取得する階層数
		 */
		public boolean write(
					AttributeTree   attrTree,
					List  			prmAttrTreeValueList,
					PrintWriter		out,
					PrintWriter		parentOut,
					int 			searchLevel
					)
		throws Exception
		{			
			if (searchLevel < attrTree.getTreeItemList().size()) {
				return writeAttrTree(attrTree, prmAttrTreeValueList, out, parentOut, searchLevel);
			} else {
				return writeFolder(attrTree, prmAttrTreeValueList, out, parentOut, searchLevel);
			}

		}

		private boolean writeAttrTree(
					AttributeTree   attrTree,
					List  			prmAttrTreeValueList,
					PrintWriter		out,
					PrintWriter		parentOut,
					int 			searchLevel
					)
		throws Exception
		{			
			boolean isAttrTreeLeaf = (searchLevel == attrTree.getTreeItemList().size() - 1);
			boolean isLastLevel = (searchLevel == prmAttrTreeValueList.size());
			
			List searchAttrTreeValueList = prmAttrTreeValueList.subList(0,searchLevel);
			//属性ツリー項目
			List attrTreeItemValueList = AttributeTreeUtil.classifyAttributeValues(_sess, attrTree, searchAttrTreeValueList);
			
			int maxnum = Integer.parseInt(EIMConfig.get("GET_DATA_MAX_NUM"));	// 最大取得件数
			
			// <最大取得件数>を超過した場合、余分な最後の1件は除去
			if (attrTreeItemValueList.size() > maxnum) {
				attrTreeItemValueList.remove(maxnum);
				// 最大取得件数超過ラベルの設定
				String[] maxNumArray = {String.valueOf(maxnum)};
				parentOut.println(" overflowLabel=\"" + EIMResource.getMessage(_sess, "EIM.LABEL.ATTRTREE.OVERFLOW", maxNumArray) + "\"");
			}
			
			Object nestVal = !isLastLevel?prmAttrTreeValueList.get(searchLevel):null;
			boolean isNestValFound = isLastLevel;
			for(Iterator i = attrTreeItemValueList.iterator(); i.hasNext();) {
				Object attrItemValue = i.next(); 
				String label = (attrItemValue == null)
						?""
						:(
								(attrItemValue instanceof Date)
								?DateUtils.getDBTzToCLTzDate(_sess, (Date)attrItemValue)
								:String.valueOf(attrItemValue)
						);
				String value = (attrItemValue == null)
				?""
						:(
							(attrItemValue instanceof Date)
							?String.valueOf(((Date)attrItemValue).getTime())
							:String.valueOf(attrItemValue)
						);

				//XML
				out.println("<node");
				out.println(" label=\"" + StringUtils.xmlEncode(label) + "\"");
				out.println(" value=\"" + StringUtils.xmlEncode(value) + "\"");
				out.println(" objTypeName=\"属性\"");
				out.println(" isLeaf=\"" + isAttrTreeLeaf + "\"");
				StringWriter ruSw = new StringWriter();
				PrintWriter ruOut = new PrintWriter(ruSw);

				if (nestVal == attrItemValue //for null
						|| (nestVal != null &&  nestVal.equals(attrItemValue)))
						isNestValFound = true;
								
				//引数値と同じ値のものは、下位をブレークダウン
				if (!isLastLevel
						&& (!isAttrTreeLeaf || attrTree.isClassifyTargetFolder()) //フォルダ分類なら、ツリー末端でもネストする
						&& (nestVal == attrItemValue //for null
								|| (nestVal != null &&  nestVal.equals(attrItemValue)) //for value
						)) {
					//属性ツリーは指定の階層までしか取得しない
					boolean isBranch = write(attrTree, prmAttrTreeValueList, ruOut, out, searchLevel + 1);
					out.println(" isBranch=\"" + isBranch + "\"");
					out.println(" isSearch=\"true\"");
				} else {
					boolean isBranch = attrTree.isClassifyTargetFolder()?true:!isAttrTreeLeaf;
					out.println(" isBranch=\"" + isBranch + "\"");
					out.println(" isSearch=\"" + !isBranch + "\"");
				}
				out.println(">");
					
				ruOut.close();
				String nestNodesXml = ruSw.toString();
				if (nestNodesXml.length() > 0) {
					out.println(nestNodesXml);
				}
				
				out.println("</node>");
			}
			if (!isNestValFound)
				throw new EIMException(_sess, "EIM.ERROR.LOGIC.ATTRTREE.NOTEXIST.ATTR.VALUE");

			return attrTreeItemValueList.size() > 0;
		}
		
		public boolean writeFolder(
				AttributeTree   attrTree,
				List  			prmAttrTreeValueList,
				PrintWriter		out,
				PrintWriter		parentOut,
				int 			searchLevel
				)
		throws Exception
		{
			boolean isLastLevel = (searchLevel == prmAttrTreeValueList.size());

			List childObjs = null;
			if (searchLevel == attrTree.getTreeItemList().size()) {
				List searchAttrTreeValueList = prmAttrTreeValueList.subList(0, searchLevel);
				//属性ツリー項目
				childObjs = AttributeTreeUtil.getClassifiedObjects(_sess, attrTree, searchAttrTreeValueList);
				
				int maxnum = Integer.parseInt(EIMConfig.get("GET_DATA_MAX_NUM"));	// 最大取得件数
				
				// <最大取得件数>を超過した場合、余分な最後の1件は除去
				if (childObjs.size() > maxnum) {
					childObjs.remove(maxnum);
					// 最大取得件数超過ラベルの設定
					String[] maxNumArray = {String.valueOf(maxnum)};
					parentOut.println(" overflowLabel=\"" + EIMResource.getMessage(_sess, "EIM.LABEL.ATTRTREE.OVERFLOW", maxNumArray) + "\"");
				}
				
			} else {
				long parentObjId = ((Long)prmAttrTreeValueList.get(searchLevel - 1)).longValue();
				EIMObject parentObj = ObjectUtils.getObjectById(_sess, parentObjId);
				childObjs = _helper.getChildObjectsInAccessibleStatus(parentObj, true);
			}
			
			long nestObjId = !isLastLevel?((Long)prmAttrTreeValueList.get(searchLevel)).longValue():-1;
			if (nestObjId != -1 && ObjectUtils.getObjectById(_sess, nestObjId) == null)
				throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOWSFOL");
			
			boolean isObjFound = isLastLevel;
			for(Iterator i = childObjs.iterator(); i.hasNext();) {
				EIMObject obj = (EIMObject)i.next(); 
			
				//XML
				out.println("<node");
				out.println(" label=\"" + StringUtils.xmlEncode(obj.getName()) + "\"");
				out.println(" isLeaf=\"false\"");
				out.println(" objId=\"" + obj.getId() + "\"");
				out.println(" objTypeId=\"" + obj.getType().getId() + "\"");
				out.println(" objTypeName=\"" + _helper.getObjTypeNameFolderXmlEscaped() + "\"");
				//ワークフロー付きフォルダであるかどうかのフラグ
				out.println(" isWorkflowFolder=\"" + _helper.isTypeOfFolderWithWorkflow(obj)+"\"");
				StringWriter ruSw = new StringWriter();
				PrintWriter ruOut = new PrintWriter(ruSw);
			
				//引数値と同じ値のものは、下位をブレークダウン
				if (!isLastLevel && (nestObjId == obj.getId())) {
					isObjFound = true;
					//再起先はフォルダ出力
					boolean isBranch = writeFolder(attrTree, prmAttrTreeValueList, ruOut, out, searchLevel + 1);
					out.println(" isBranch=\"" + isBranch + "\"");	// ブランチに全て△(子フォルダあり)を設定
					out.println(" isSearch=\"true\"");	// 探索済みフラグをfalseに設定
				} else {
					out.println(" isBranch=\"true\"");	// ブランチに全て△(子フォルダあり)を設定
					out.println(" isSearch=\"false\"");	// 探索済みフラグをfalseに設定
				}
				out.println(">");
					
				ruOut.close();
				String nestNodesXml = ruSw.toString();
				if (nestNodesXml.length() > 0) {
					out.println(nestNodesXml);
				}
				
				out.println("</node>");
			}
			if (!isObjFound)
				throw new EIMException(_sess, "EIM.ERROR.LOGIC.ATTRTREE.NOTEXIST.FOLDAER");
			
			return childObjs.size() > 0;
		}
	}

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;

	// Parameter
	//属性ツリー情報の取得
	AttributeTreeUtil.HttpParamParsed param = AttributeTreeUtil.parseHttpParam(request);
	
	//Message
	String message = null;
	Object[] paramId = param.getParamIds();
	
	try
	{
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

		AttributeTree openAttrTree = null;
		try {
			//パラメータのチェックと属性ツリーの取得
			openAttrTree = AttributeTreeUtil.checkAndGetAttrTree(sess, param);
		} catch (EIMException e) {
			if (!AttributeTreeUtil.isErrorOfCheckAndGetAttrTree(e))
				throw e;
			message = e.getMessage();
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage(e.getMessageKey());
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		//属性値数のチェック
		if ((openAttrTree != null)
				&& openAttrTree.isClassifyTargetDocument()
				&& (param.getAttrTreeValues().size() > openAttrTree.getTreeItemList().size())) {
			message = EIMResource.getMessage(request, "EIM.ERROR.LOGIC.ATTRTREE.MANY_OR_LESS.VALUES");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRTREE.MANY_OR_LESS.VALUES");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		//属性ツリー項目を持つ属性ツリーリストを取得する。
		List attrTreeList = AttributeTreeUtil.getAttributeTreeListOnlyHavingItem(sess);
		
		//1件も登録が無ければ、ここで終了
		if (attrTreeList.size() == 0) {
			out.println("<nodes></nodes>");
			return;
		}

		//パラメータで属性ツリーの指定がなければ、リスト上最初の属性ツリーをデフォルト展開する
		if (openAttrTree == null)
			openAttrTree = (AttributeTree)attrTreeList.get(0);

		//属性ツリー値の型変換
		List prmAttrTreeValueList = param.getAttrTreeValues();
		AttributeTreeUtil.convertAttrTreeValueToDataType(openAttrTree, prmAttrTreeValueList);

		//処理途中で妥当性チェック例外が発生するため、出力はメモリに書いて最後に一気にoutに書く
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		//Root Node
		pw.println("<nodes>");
		for(Iterator i = attrTreeList.iterator(); i.hasNext();)
		{
			AttributeTree attrTree = (AttributeTree)i.next();
			//XML
			pw.println("<node");
			pw.println(" label=\"" + StringUtils.xmlEncode(attrTree.getName()) + "\"");
			pw.println(" objTypeName=\"属性\"");
			pw.println(" attrTreeId=\"" + attrTree.getId() + "\"");
			pw.println(" classifyTarget=\"" + attrTree.getClassifyTarget() + "\"");
			pw.println(" attrTreeSettings=\"" + AttributeTreeUtil.getAttrTreeSettingsStr(sess, attrTree) + "\"");
			StringWriter ruSw = new StringWriter();
			PrintWriter ruOut = new PrintWriter(ruSw);
			if (attrTree.getId() == openAttrTree.getId()) {
				//下位を検索する属性ツリーの場合はRecurrentUtilで下位データを作成する
				//Recurrently
				RecurrentUtils ru = new RecurrentUtils(sess);
				try {
					boolean isBranch = ru.write(openAttrTree, prmAttrTreeValueList, ruOut, pw, 0);
					pw.println(" isBranch=\"" + isBranch + "\"");	// ブランチに全て△(子フォルダあり)を設定
				} catch (EIMException e) {
					String messageKey = e.getMessageKey();
					if (!messageKey.equals("EIM.ERROR.LOGIC.NOWSFOL")
							&& !messageKey.equals("EIM.ERROR.LOGIC.ATTRTREE.NOTEXIST.FOLDAER")
							&& !messageKey.equals("EIM.ERROR.LOGIC.ATTRTREE.NOTEXIST.ATTR.VALUE")
							)
						throw e;
					out.clear();
					message = e.getMessage();
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage(e.getMessageKey());
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
				pw.println(" isSearch=\"true\"");	// 一番上の属性ツリーのみ探索済みフラグをtrueに設定
			} else {
				pw.println(" isBranch=\"true\"");	// ブランチに全て△(子フォルダあり)を設定
				pw.println(" isSearch=\"false\"");	// 探索済みフラグをfalseに設定
			}
			pw.println(">");
			
			ruOut.close();
			String nestNodesXml = ruSw.toString();
			if (nestNodesXml.length() > 0)
				pw.println(nestNodesXml);
			
			//End XML
			pw.println("</node>");
		}

		//End Root Node
		pw.println("</nodes>");
		pw.close();
		out.print(sw.toString());

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