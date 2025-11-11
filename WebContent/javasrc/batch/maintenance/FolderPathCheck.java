package batch.maintenance;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectUtil;

import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMSecurity;
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

public class FolderPathCheck {
	/**
	 * ログ
	 */
	public static Log log = LogFactory.getLog(FolderPathCheck.class);

	/** エスケープ済ダブルクォーテーション */
	private static final String ESCAPE_DQ = AppConstant.CSV_ESCDQUOTATION + "\"";

	/**
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		Connection con = null;
		EIMSession sess = null;
		try {
			//Console Session
			sess = new EIMSession();
			sess.setConsole();

			// コネクション取得
			con = sess.getDBConnection();

			log.info("--- WS/フォルダのセキュリティ確認スクリプト開始 ---");
			log.info("--- 2022/3/17差替え版 ---");

			// フォルダタイプ取得
			EIMObjectType folderType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));
			// フォルダタイプ配下のタイプを全て取得する
			List folderTypeList = new ArrayList();
			ObjectUtils.getChildObjectTypeListRecurrently(sess, folderType, folderTypeList);
			folderTypeList.add(folderType);

			// セキュリティ取得
			List<EIMSecurity> secList = SecurityUtils.getSecurityList(sess);
			Map<Long, EIMSecurity> secMap = new HashMap<Long, EIMSecurity>();
			for (EIMSecurity sec : secList) {
				secMap.put((long)sec.getId(), sec);
			}

			// 全てのフォルダとセキュリティを取得
			List<FolderDomain> resultFolderList = new ArrayList<FolderDomain>();
			String sql = "select eobj.id id, eobj.name name , ostr.value path, eobj.security  securityId "
					+ "from EIMOBJ eobj, EIMOBJSTR ostr "
					+ "where eobj.type = ? "
					+ "and eobj.id = ostr.id "
					+ "and ostr.type = ( select id from EIMATTR where name = 'パス' ) "
					+ "and ostr.key = 1";

			PreparedStatement ps = null;
			ResultSet rs = null;
			ps = sess.getDBConnection().prepareStatement(sql);

			for (int i = 0; i < folderTypeList.size(); i++){
				EIMObjectType objType = (EIMObjectType)folderTypeList.get(i);
				// SQL実行
				ps.setLong(1, objType.getId());
				rs = ps.executeQuery();

				while (rs.next()) {
					long id = rs.getInt("id");
					String name = rs.getString("name");
					String path = rs.getString("path");
					int securityId = rs.getInt("securityId");

					FolderDomain folderResult = new FolderDomain();
					// ID
					folderResult.setObjID(id);
					//パス
					folderResult.setPath(path + name);
					//名称
					folderResult.setObjName(name);
					//セキュリティ
					if (secMap.get(securityId)!= null) {
						folderResult.setSecurity(secMap.get(securityId).getName());
					} else {
						folderResult.setSecurity("");
					}

					resultFolderList.add(folderResult);
				}
				rs.close();
			}
			ps.cancel();


			// ワークスぺース情報を取得
			EIMObjectType workspaceType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"));
			List workspaceList = ObjectUtils.getObjectListByType(sess, workspaceType);
			for(int k = 0; k < workspaceList.size(); k++){
				EIMObject workspace = (EIMObject)workspaceList.get(k);

				FolderDomain folderResult = new FolderDomain();
				//ID
				folderResult.setObjID(workspace.getId());
				//パス
				folderResult.setPath("/"+workspace.getName());
				//名称
				folderResult.setObjName(workspace.getName());
				//セキュリティ
				if(workspace.getSecurity() != null){
					folderResult.setSecurity(workspace.getSecurity().getName());
				}
				resultFolderList.add(folderResult);
			}

			// パスで並び替える
			List sortedResultList = AppObjectUtil.getStrSortedList(resultFolderList,"getPath",true);


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
			log.info(" --- フォルダのセキュリティ確認スクリプト終了 ---");
		}
	}

	/**
	 * CSV出力処理
	 *
	 * @param FolderList
	 * @throws Exception
	 */
	private static void outputList(List<FolderDomain> folderList) throws Exception {

		log.info("CSV出力処理開始");

		try
		{
			MessageFormat mf = new MessageFormat("{0,date,yyyyMMddHHmmss}");
			Object[] objs = {Calendar.getInstance().getTime()};

			String FileName = "WSFolderSecurityCheck_" + mf.format(objs) + ".csv";

			FileWriter outFile = new FileWriter(FileName);

			log.info("OUTPUT_PATH = " + FileName);

			BufferedWriter outBuffer = new BufferedWriter(outFile);

			outBuffer.write("WSフォルダID" + "," +"WSフォルダ名"+ "," +"WSフォルダパス" + "," + "WSフォルダセキュリティ名");

			outBuffer.newLine();

			for (int i = 0; i < folderList.size(); i++)
			{
				FolderDomain result = folderList.get(i);

				//WSFolderID
				String WSFolderID = Long.toString(result.getObjID());

				//WSFolderName
				String WSFolderName = escString(result.getObjName());

				//WSFolderPath
				String WSFolderPath = escString(result.getPath());

				//WSFolderSec
				String WSFolderSec = escString(result.getSecurity());

				outBuffer.write(WSFolderID + "," + WSFolderName + "," + WSFolderPath + "," + WSFolderSec );
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
