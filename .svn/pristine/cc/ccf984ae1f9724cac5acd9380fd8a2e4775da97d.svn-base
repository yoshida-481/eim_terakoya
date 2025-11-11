package batch.maintenance;

import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import common.util.AppMessageUtils;
import eim.bo.EIMGroup;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.GroupUtils;
import java.text.MessageFormat;
import java.util.*;
import java.io.BufferedWriter;
import java.io.FileWriter;
import eim.util.*;

public class UserGroupList {

	/***********************************************************************
	 *
	 *
	 * ユーザがどのグループに所属しているかCSV出力する
	 *
	 *
	 * **********************************************************************/

	/**
	 * ログ
	 */
	public static Log log = LogFactory.getLog(UserGroupList.class);

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

			log.info("--- UserGroupList Script Start ---");
			System.out.println("--- UserGroupList Script Start ---");

			//全ユーザを検索対象とする
			List userList = null;
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
				String FileName = "[UserGroupList]" + mf.format(objs) + ".csv";
				FileWriter outFile = new FileWriter(FileName);
				BufferedWriter outBuffer = new BufferedWriter(outFile);
				outBuffer.write("CodeNo" + "," + "userName" + "," + "groupPath" + "," + "deleteFLG");
				outBuffer.newLine();
				System.out.println(FileName);

				//CodeNo+ユーザ名＋グループ名の宣言
				String userName = "";
				String codeNo = "";
				String groupPath = "";
				String deleteFLG = "";

				for(int i = 0; i < userList.size(); i++)
				{
					//User
					EIMUser user = (EIMUser)userList.get(i);


					if (user != null) {
						//所属グループ名称の取得
						List groupList = GroupUtils.getGroupByUser(sess, user);
						for (int BelongGroup = 0; BelongGroup < groupList.size(); BelongGroup++) 	{
							EIMGroup group = (EIMGroup)groupList.get(BelongGroup);

							//Groupの階層を取得する
							group = GroupUtils.getGroupById(sess, group.getId());
							while(group != null)
							{
								if(group.getParent() != null)
								{
									groupPath = " / " + group.getName() + groupPath;
								}
								else
								{
									groupPath = group.getName() + groupPath;
								}
								group = group.getParent();
							}
							/*
							 * CSV出力：CodeNo+ユーザ名＋所属グループ名+削除フラグを一行ずつ出力する
							 * */
							if(user.getDisable() == 0){
								deleteFLG ="有効";
							}else{
								deleteFLG ="無効";
							}
							codeNo = user.getCode();
							userName = user.getName();
							outBuffer.write(codeNo + "," + userName + "," + groupPath +"," + deleteFLG);
							outBuffer.newLine();
							groupPath = "";
						}
					}
				}
				outBuffer.flush();
				outBuffer.close();
			}

		}
		catch(Exception ee)
		{
			log.error(AppMessageUtils.makeLogMessage(ee.getMessage()), ee);
		}
		finally {
			try{
				if(sess != null){
					sess.close();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
			log.info("--- UserGroupList Script Finish ---");
			System.out.println("--- UserGroupList Script Finish ---");
		}
	}
}
