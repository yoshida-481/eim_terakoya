package batch.maintenance;

import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import common.util.AppMessageUtils;
import eim.bo.EIMRole;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.RoleUtils;

import java.text.MessageFormat;
import java.util.*;
import java.io.BufferedWriter;
import java.io.FileWriter;
import eim.util.*;

public class UserRoleList {

	/***********************************************************************
	 *
	 *
	 * ユーザがどのロールを保持しているかCSV出力する
	 *
	 *
	 * **********************************************************************/

	/**
	 * ログ
	 */
	public static Log log = LogFactory.getLog(UserRoleList.class);

	/**
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		EIMSession sess = null;
		try {

			//Console Session
			sess = new EIMSession();
			sess.setConsole();
			EIMThreadContext.putEIMSession(sess);


			log.info("--- UserRolepList Script Start ---");
			System.out.println("--- UserRolepList Script Start ---");


			List userList = null;
			//全ユーザを検索対象とする
			userList = UserUtils.searchUser(	sess,
												"",
												"",
												"",
												null,
												false);

			// 検索結果が0件以外の時
			if(userList.size() != 0)
			{
				System.out.println("出力対象ユーザ数 : " + userList.size());
				/*
				 * CSV出力準備
				 * ファイル名とヘッダーの指定
				 */
				MessageFormat mf = new MessageFormat("{0,date,yyyyMMddHHmmss}");
				Object[] objs = {Calendar.getInstance().getTime()};
				String FileName = "[UserRoleList]" + mf.format(objs) + ".csv";
				FileWriter outFile = new FileWriter(FileName);
				BufferedWriter outBuffer = new BufferedWriter(outFile);
				outBuffer.write("CodeNo" + "," + "userName" + "," + "roleName" + "deleteFLG");
				outBuffer.newLine();

				//codeNo+ユーザ名＋ロール名の宣言
				String userName = "";
				String codeNo = "";
				String roleName = "";
				String deleteFLG = "";

				for(int i = 0; i < userList.size(); i++)
				{
					//User
					EIMUser user = (EIMUser)userList.get(i);

					if (user != null) {
						//所属ロール名称の取得
						List roleList = RoleUtils.getRoleByUser(sess, user);
						for (int BelongROle = 0; BelongROle < roleList.size(); BelongROle++) {
							EIMRole role = (EIMRole)roleList.get(BelongROle);

							/*
							 * CSV出力：PinNo+ユーザ名＋ロール名+削除フラグを一行ずつ出力する
							 * */
							if(user.getDisable() == 0){
								deleteFLG ="有効";
							}else{
								deleteFLG ="無効";
							}
							codeNo = user.getCode();
							userName = user.getName();
							roleName = role.getName();
							outBuffer.write(codeNo + "," + userName + "," + roleName +"," + deleteFLG);
							outBuffer.newLine();
						}
					}
				}
				outBuffer.flush();
				outBuffer.close();
			}
		} catch (Exception ee) {
			log.error(AppMessageUtils.makeLogMessage(ee.getMessage()), ee);
		} finally {
			try{
				if(sess != null){
					sess.close();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
			log.info("--- UserRoleList Batch Finish ---");
			System.out.println("--- UserRoleList Batch Finish ---");
		}
	}
}
