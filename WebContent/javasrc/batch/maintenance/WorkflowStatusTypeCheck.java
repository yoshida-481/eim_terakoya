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
import jp.co.ctc_g.eim.framework2.business.domain.criteria.StatusTypeCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AssignmentEntryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain;
import jp.co.ctc_g.eim.framework2.business.service.StatusTypeService;
import jp.co.ctc_g.eim.framework2.business.service.UserService;
import jp.co.ctc_g.eim.framework2.business.service.WorkflowService;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;
import jp.co.ctc_g.eim.framework2.common.enumeration.EntryTypeEnum;

public class WorkflowStatusTypeCheck {
	/**
	 * ログ
	 */
	public static Log log = LogFactory.getLog(WorkflowStatusTypeCheck.class);

	/** エスケープ済ダブルクォーテーション */
	private static final String ESCAPE_DQ = AppConstant.CSV_ESCDQUOTATION + "\"";
	
	/** ワークフローサービス */
	private static WorkflowService workflowService = null;
	/** ステータスタイプサービス */
	private static StatusTypeService statusTypeService = null;
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
			statusTypeService = (StatusTypeService)context.getBean("statusTypeService2");
			
			UserDomain user = userService.getById(1);
			tc.setUser(user);

			log.info("--- WorkflowCheck確認スクリプト開始 ---");
			
			// ワークフロー取得
			//List<EIMWorkFlow> workflowList = WorkFlowUtils.getWorkFlowList(sess);
			List<WorkflowDomain> workflowList = workflowService.getList();
			List<MaintenanceWorkflowDomain> resultList = new ArrayList<MaintenanceWorkflowDomain>();
			for (WorkflowDomain  workflow : workflowList) {
				StatusTypeCriteria  criteria = new StatusTypeCriteria();
				criteria.setWorkflowId(workflow.getId());
				List<StatusTypeDomain> statusTypeList = statusTypeService.getList(criteria);
				for (StatusTypeDomain status : statusTypeList) {
					if (status.getAssignmentEntryList() != null && status.getAssignmentEntryList().size() > 0) {
						for (AssignmentEntryDomain assaign : status.getAssignmentEntryList()) {
							MaintenanceWorkflowDomain resultDomain = new MaintenanceWorkflowDomain();
							resultDomain.setWorkflowID(workflow.getId());
							resultDomain.setWorkflowName(workflow.getName());
							resultDomain.setRevId(workflow.getRevisionGroupId());
							resultDomain.setRev(workflow.getRevision());
							resultDomain.setStatusTypeName(status.getName());
							if (assaign.getEntryType().equals(EntryTypeEnum.USER)) {
								resultDomain.setAssignKind("ユーザ");
							} else if (assaign.getEntryType().equals(EntryTypeEnum.GROUP)) {
								resultDomain.setAssignKind("グループ");
							} else if (assaign.getEntryType().equals(EntryTypeEnum.ROLE)) {
								resultDomain.setAssignKind("ロール");
							} else if (assaign.getEntryType().equals(EntryTypeEnum.COMPLEX)) {
								resultDomain.setAssignKind("復号グループ");
							}
							resultDomain.setAssignName(assaign.getEntryElement().getName());
							resultList.add(resultDomain);
						}
						
					} else {
						MaintenanceWorkflowDomain resultDomain = new MaintenanceWorkflowDomain();
						resultDomain.setWorkflowID(workflow.getId());
						resultDomain.setWorkflowName(workflow.getName());
						resultDomain.setRevId(workflow.getRevisionGroupId());
						resultDomain.setRev(workflow.getRevision());
						resultDomain.setStatusTypeName(status.getName());
						resultList.add(resultDomain);
					}
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

			String FileName = "WorkFlowStatusCheck_" + mf.format(objs) + ".csv";

			FileWriter outFile = new FileWriter(FileName);

			log.info("OUTPUT_PATH = " + FileName);

			BufferedWriter outBuffer = new BufferedWriter(outFile);

			outBuffer.write("ワークフローID" + "," +"ワークフロー名"+ "," +"ワークフローrebGroup" + ", " +"ワークフローrev" + "," + "ステータスタイプ名" + "," + "アサイン種別" +"," + "アサイン名");

			outBuffer.newLine();

			for (int i = 0; i < list.size(); i++)
			{
				MaintenanceWorkflowDomain result = list.get(i);

				String id = Long.toString(result.getWorkflowID());
				String name = escString(result.getWorkflowName());
				String rev = Long.toString(result.getRev());
				String revId = Long.toString(result.getRevId());
				String status = escString(result.getStatusTypeName());
				String kind = escString(result.getAssignKind());
				String assign = escString(result.getAssignName());

				outBuffer.write(id + "," + name + "," + revId + "," + rev + "," + status + "," + kind + "," + assign );
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
