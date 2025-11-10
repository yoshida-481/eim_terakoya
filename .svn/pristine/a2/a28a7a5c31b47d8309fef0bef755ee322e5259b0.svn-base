package batch.maintenance;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import eim.bo.EIMSecurity;
import eim.net.EIMSession;
import eim.util.SecurityUtils;
import java.text.MessageFormat;
import java.util.*;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;


import batch.maintenance.domain.*;
import eim.util.StringUtils;
import eim.bo.EIMAccessEntry;
import eim.bo.EIMAccessEntryType;
import eim.bo.EIMAccessRole;

public class SecurityCheck {
	/**
	 * ログ
	 */
	public static Log log = LogFactory.getLog(SecurityCheck.class);

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

			log.info("--- セキュリティのエントリー確認スクリプト開始 ---");

			/*-------全セキュリティ情報を格納する箱-------*/
			List<SecurityInfoDomain> SecurityInfoList = new ArrayList<SecurityInfoDomain>();

			//セキュリティのリスト取得
			List secList = SecurityUtils.getSecurityList(sess);

			//セキュリティのエントリーを確認する
			for(int s = 0; s < secList.size() ; s++)
			{
				//セキュリティリストからセキュリティを取得
				EIMSecurity sec = (EIMSecurity)secList.get(s);

				//セキュリティID
				String SecurityID = Long.toString(sec.getId());

				//セキュリティ名
				String SecurityName = sec.getName();


				//エントリーリストを取得
				List entryList = SecurityUtils.getAccessEntryList(sess, sec);

				for(int e = 0;e < entryList.size() ; e++)
				{
					//エントリー
					EIMAccessEntry entry = (EIMAccessEntry)entryList.get(e);
					String EntryName = "";

					//ユーザ
					if(entry.getType().getId() == EIMAccessEntryType.USER)
					{
						EntryName = StringUtils.xmlEncode(entry.getUser().getName());
					}
					//グループ
					else if(entry.getType().getId() == EIMAccessEntryType.GROUP)
					{
						EntryName = StringUtils.xmlEncode(entry.getGroup().getName());
					}
					//ロール
					else if(entry.getType().getId() == EIMAccessEntryType.ROLE)
					{
						EntryName = StringUtils.xmlEncode(entry.getRole().getName());
					}
					//複合グループ
					else if(entry.getType().getId() == EIMAccessEntryType.COMP)
					{
						EntryName = StringUtils.xmlEncode(entry.getComp().getName());
					}
					//ユーザ定義
					else if(entry.getType().getId() == EIMAccessEntryType.USERDEF)
					{
						EntryName = StringUtils.xmlEncode(entry.getUserDefGroup().getName());
					}

					//優先度
					String EntryPriority = Integer.toString(entry.getPriority());

					// RoleList
					List defaultRoleList = SecurityUtils.getAccessRoleList(sess, entry, null);

					String Role_Create ="";
					String Role_STATUS_UP ="";
					String ROLE_500 ="";
					String Role_READ ="";

					// アクセスロールの数
					for(int i = 0; i < defaultRoleList.size(); i++) {
						EIMAccessRole role = (EIMAccessRole)defaultRoleList.get(i);
						String roleName = StringUtils.xmlEncode(role.getType().getName());
						String permit = Integer.toString(role.getPermit().getValue());

						if(roleName.equals("CREATE")){
							Role_Create = permit;
						}
						else if(roleName.equals("STATUS_UP")){
							Role_STATUS_UP = permit;
						}
						else if(roleName.equals("ROLE_500")){
							ROLE_500 = permit;
						}
						else if(roleName.equals("READ")){
							Role_READ = permit;
						}
					}

					//セキュリティ情報を格納する
					SecurityInfoDomain SecurityInfo = new SecurityInfoDomain();
					SecurityInfo.setSecurityID(SecurityID);
					SecurityInfo.setSecurityName(SecurityName);
					SecurityInfo.setSecurityEntry(EntryName);
					SecurityInfo.setEntryPriority(EntryPriority);
					SecurityInfo.setRole_Create(Role_Create);
					SecurityInfo.setRole_STATUS_UP(Role_STATUS_UP);
					SecurityInfo.setROLE_500(ROLE_500);
					SecurityInfo.setRole_READ(Role_READ);

					SecurityInfoList.add(SecurityInfo);
				}
			}

			/*-------出力する-------*/
			outputList(SecurityInfoList);

		} catch (Exception ee) {
			System.out.println(ee.getMessage());
		} finally {
			try{
			}
			catch (Exception se) {
				System.out.println(se.getMessage());
			}
			log.info("--- セキュリティのエントリー確認スクリプト終了 ---");
		}
	}

	/*-----------出力処理-------------*/
	private static void outputList(List<SecurityInfoDomain>SecurityInfoList) throws Exception {

		log.info("CSV出力処理開始");

		try
		{
			MessageFormat mf = new MessageFormat("{0,date,yyyyMMddHHmmss}");
			Object[] objs = {Calendar.getInstance().getTime()};

			String FileName = "SecurityInfoCheck_" + mf.format(objs) + ".csv";

			FileWriter outFile = new FileWriter( FileName);

			log.info("OUTPUT_PATH = "+ FileName);

			BufferedWriter outBuffer = new BufferedWriter(outFile);

			outBuffer.write("セキュリティID" + "," +"セキュリティ名"+ "," +"エントリー" + "," + "優先度" + "," + "書込" + "," +"ステータス変更"+ "," +"常時読取" + "," + "公開読取");

			outBuffer.newLine();

			for (int i = 0; i < SecurityInfoList.size(); i++)
			{
				SecurityInfoDomain Result = SecurityInfoList.get(i);

				//SecurityID
				String SecurityID = Result.getSecurityID();

				//SecurityName
				String SecurityName = Result.getSecurityName();

				//SecurityEntry
				String SecurityEntry = Result.getSecurityEntry();

				//EntryPriority
				String EntryPriority = Result.getEntryPriority();

				//書込み
				String Create = "";
				if(Result.getRole_Create().equals("1")){
					Create = "許可";
				}else if(Result.getRole_Create().equals("0")){
					Create  = "拒否";
				}else if(Result.getRole_Create().equals("2")){
					Create = "無視";
				}


				//ステータス変更
				String STATUS_UP = "";
				if(Result.getRole_STATUS_UP().equals("1")){
					STATUS_UP = "許可";
				}else if(Result.getRole_STATUS_UP().equals("0")){
					STATUS_UP  = "拒否";
				}else if(Result.getRole_STATUS_UP().equals("2")){
					STATUS_UP = "無視";
				}

				//常時読取
				String AlltimeRead = "";
				if(Result.getROLE_500().equals("1")){
					AlltimeRead = "許可";
				}else if(Result.getROLE_500().equals("0")){
					AlltimeRead  = "拒否";
				}else if(Result.getROLE_500().equals("2")){
					AlltimeRead = "無視";
				}

				//公開読取
				String OpenRead = "";
				if(Result.getRole_READ().equals("1")){
					OpenRead = "許可";
				}else if(Result.getRole_READ().equals("0")){
					OpenRead  = "拒否";
				}else if(Result.getRole_READ().equals("2")){
					OpenRead = "無視";
				}

				outBuffer.write( SecurityID + "," + SecurityName + "," + SecurityEntry + "," + EntryPriority + "," + Create + "," + STATUS_UP + "," + AlltimeRead + "," + OpenRead);
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
}
