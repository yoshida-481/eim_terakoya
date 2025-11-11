package batch.maintenance;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectUtil;

import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMSecurity;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import java.text.MessageFormat;
import java.util.*;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.*;

import batch.maintenance.domain.*;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain;
import jp.co.ctc_g.eim.framework2.business.service.UserService;
import jp.co.ctc_g.eim.framework2.business.service.WorkflowService;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;

public class WorkflowTypeCheck {
	/**
	 * ログ
	 */
	public static Log log = LogFactory.getLog(WorkflowTypeCheck.class);

	/** エスケープ済ダブルクォーテーション */
	private static final String ESCAPE_DQ = AppConstant.CSV_ESCDQUOTATION + "\"";
	
	/** ワークフローサービス */
	private static WorkflowService workflowService = null;
	/** ユーザサービス */
	private static UserService userService = null;

	/**
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		Connection con = null;
		EIMSession sess = null;
		try {
			//lang取得
			String lang = "";
			String EIM_CONFIG_LANG = "MESSAGELANG";
			String DEFAULT_LANG    = "JA";
			if(EIMConfig.get(EIM_CONFIG_LANG) != null){
				lang = EIMConfig.get(EIM_CONFIG_LANG);
			}else{
				lang = DEFAULT_LANG;
			}
			//Console Session
			sess = new EIMSession();
			sess.setConsole();

			// コネクション取得
			con = sess.getDBConnection();
			
			// トランザクション取得
			ApplicationContext context = ApplicationContextLoader.getContext();
			TransactionContext tc = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
			EIMThreadContext.putTransactionContext(tc);
			tc.setLangId(lang);
			tc.setDBConnection(sess.getDBConnection());
			workflowService = (WorkflowService)context.getBean("workflowService2");
			userService = (UserService)context.getBean("userService2");
			UserDomain user = userService.getById(1);
			tc.setUser(user);

			log.info("--- WorkflowCheck確認スクリプト開始 ---");
			
			// フォルダタイプ取得
			EIMObjectType folderType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));
			// フォルダタイプ配下のタイプを全て取得する
			List<EIMObjectType> folderTypeList = new ArrayList<EIMObjectType>();
			ObjectUtils.getChildObjectTypeListRecurrently(sess, folderType, folderTypeList);
			folderTypeList.add(folderType);
			
			// ドキュメントタイプ取得
			EIMObjectType documentType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
			List<EIMObjectType> documentTypeList = new ArrayList<EIMObjectType>();
			ObjectUtils.getChildObjectTypeListRecurrently(sess, documentType, documentTypeList);
			documentTypeList.add(documentType);
			
			// ドキュメントタイプ取得
			EIMObjectType formType = ObjectUtils.getObjectTypeByName(sess, "app.form.dev:帳票");
			List<EIMObjectType> formTypeList = new ArrayList<EIMObjectType>();
			ObjectUtils.getChildObjectTypeListRecurrently(sess, formType, formTypeList);
			formTypeList.add(formType);
			
			// ワークフロー毎のMapに変換
			HashMap<Long, List<EIMObjectType>> typeMap = new HashMap<Long, List<EIMObjectType>>();
			for (EIMObjectType type : folderTypeList) {
				EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByType(sess, type);
				if (workFlow != null) {
					long workFlowId = workFlow.getId();
					if (typeMap.containsKey(workFlowId)) {
						List<EIMObjectType> addList = typeMap.get(workFlowId);
						addList.add(type);
						typeMap.put(workFlowId, addList);
					} else {
						List<EIMObjectType> addList = new ArrayList<EIMObjectType>();
						addList.add(type);
						typeMap.put(workFlowId, addList);
					}
				}
			}
			
			for (EIMObjectType type : documentTypeList) {
				EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByType(sess, type);
				if (workFlow != null) {
					long workFlowId = workFlow.getId();
					if (typeMap.containsKey(workFlowId)) {
						List<EIMObjectType> addList = typeMap.get(workFlowId);
						addList.add(type);
						typeMap.put(workFlowId, addList);
					} else {
						List<EIMObjectType> addList = new ArrayList<EIMObjectType>();
						addList.add(type);
						typeMap.put(workFlowId, addList);
					}
				}
			}
			
			for (EIMObjectType type : formTypeList) {
				EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByType(sess, type);
				if (workFlow != null) {
					long workFlowId = workFlow.getId();
					if (typeMap.containsKey(workFlowId)) {
						List<EIMObjectType> addList = typeMap.get(workFlowId);
						addList.add(type);
						typeMap.put(workFlowId, addList);
					} else {
						List<EIMObjectType> addList = new ArrayList<EIMObjectType>();
						addList.add(type);
						typeMap.put(workFlowId, addList);
					}
				}
			}
			
			// ワークフロー取得
			//List<EIMWorkFlow> workflowList = WorkFlowUtils.getWorkFlowList(sess);
			List<WorkflowDomain> workflowList = workflowService.getList();
			List<MaintenanceWorkflowDomain> resultList = new ArrayList<MaintenanceWorkflowDomain>();
			for (WorkflowDomain  workflow : workflowList) {
				
				if (typeMap.containsKey(workflow.getId())) {
					for (EIMObjectType type : typeMap.get(workflow.getId())) {
						MaintenanceWorkflowDomain resultDomain = new MaintenanceWorkflowDomain();
						resultDomain.setWorkflowID(workflow.getId());
						resultDomain.setWorkflowName(workflow.getName());
						resultDomain.setRevId(workflow.getRevisionGroupId());
						resultDomain.setRev(workflow.getRevision());
						resultDomain.setType(type.getDefName());
						resultList.add(resultDomain);
					}
				} else {
					MaintenanceWorkflowDomain resultDomain = new MaintenanceWorkflowDomain();
					resultDomain.setWorkflowID(workflow.getId());
					resultDomain.setWorkflowName(workflow.getName());
					resultDomain.setRevId(workflow.getRevisionGroupId());
					resultDomain.setRev(workflow.getRevision());
					resultDomain.setType("");
					resultList.add(resultDomain);
				}
			}

			// リビジョングループIDで並び替える
			List<MaintenanceWorkflowDomain> sortedResultList = AppObjectUtil.getLongSortedList(resultList,"getRevId",true);


			/*-------出力する-------*/
			outputList(sortedResultList);

		}catch(EIMException eime){
			log.warn(AppMessageUtils.makeLogMessage(eime.getMessage()), eime);
			try{
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		catch(Exception e)
		{
			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			try{
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		finally
		{
			try{
				if(sess != null){
					sess.close();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
			//  フォルダのセキュリティ確認スクリプト終了
			log.info(" --- WorkflowCheck確認スクリプト終了 ---");
		}
	}

	/**
	 * CSV出力処理
	 *
	 * @param FolderList
	 * @throws Exception
	 */
	private static void outputList(List<MaintenanceWorkflowDomain> list) throws Exception {

		log.info("CSV出力処理開始");

		try
		{
			MessageFormat mf = new MessageFormat("{0,date,yyyyMMddHHmmss}");
			Object[] objs = {Calendar.getInstance().getTime()};

			String FileName = "WorkFlowTypeCheck_" + mf.format(objs) + ".csv";

			FileWriter outFile = new FileWriter(FileName);

			log.info("OUTPUT_PATH = " + FileName);

			BufferedWriter outBuffer = new BufferedWriter(outFile);

			outBuffer.write("ワークフローID" + "," +"ワークフロー名"+ "," +"ワークフローrebGroup" + ", " +"ワークフローrev" + "," + "割当タイプ名");

			outBuffer.newLine();

			for (int i = 0; i < list.size(); i++)
			{
				MaintenanceWorkflowDomain result = list.get(i);

				String id = Long.toString(result.getWorkflowID());
				String name = escString(result.getWorkflowName());
				String rev = Long.toString(result.getRev());
				String revId = Long.toString(result.getRevId());
				String type = escString(result.getType());

				outBuffer.write(id + "," + name + "," + revId + "," + rev + "," + type );
				outBuffer.newLine();
			}

			outBuffer.flush();
			outBuffer.close();
		}
		catch (FileNotFoundException e)
		{
			e.printStackTrace();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
	}

	/**
	 * 文字列をエスケープする。<BR>
	 * エスケープ方法は、Excel2003に従う。
	 * @param str String
	 *
	 * @return String エスケープしてダブルクォーテーションで囲んだ文字列
	 */
	private static String escString(String str)
	{
		if(str == null)
			return "\"\"";

		str = StringUtils.convertReturnCede(str);
		str = str.replaceAll("\"", ESCAPE_DQ);

		return "\"" + str + "\"";
	}

}
