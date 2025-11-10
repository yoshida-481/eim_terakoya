package app.document.approve;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.context.ApplicationContext;

import addon.PublishCommandAddOn;
import app.document.search.EIMDocSearchType;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectTypeUtil;
import common.util.AppObjectUtil;
import common.util.AppSearchUtils;
import common.util.PublishAddonUtils;
import eim.bo.EIMAccessEntry;
import eim.bo.EIMAccessEntryType;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttributeType;
import eim.bo.EIMComp;
import eim.bo.EIMException;
import eim.bo.EIMGroup;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRole;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.CompUtils;
import eim.util.EIMConfig;
import eim.util.GroupUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RoleUtils;
import eim.util.SearchUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.UserUtils;
import jp.co.ctc_g.eim.framework.business.domain.BelongDomain;
import jp.co.ctc_g.eim.framework.business.domain.BelongType;
import jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.TaskDomain;
import jp.co.ctc_g.eim.framework.business.service.TaskService;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;


/**
*
* 承認関連ユーティリティクラス
*
*/
public 	class ApproveCommonUtil
{

	/**
	 * 「承認者種別:ID」の形式の文字列から該当ユーザリストを取得して返します
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param code 「承認者種別:ID」の形式の文字列
	 * @return 該当ユーザリスト（EIMUserのList）
	 * @throws Exception
	 */
	public 	static List getUserFromCode(EIMSession sess, String code) throws Exception
	{
		List result = new ArrayList();

		long id = Long.parseLong(code.substring(2));
		switch (Integer.parseInt(code.substring(0, 1)))
		{
		case 1:
			result.add(UserUtils.getUserById(sess, id));
			break;
		case 2:
			result.addAll(GroupUtils.getUserListRecurrently(sess, GroupUtils.getGroupById(sess, id)));
			break;
		case 3:
			result.addAll(RoleUtils.getUserList(sess, RoleUtils.getRoleById(sess, id)));
			break;
		case 4:
			result.addAll(CompUtils.getUserList(sess, CompUtils.getCompById(sess, id)));
			break;
		}
		return(result);
	}
	
	/**
	 * BelongDomainから所属ユーザリストを取得して返します
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param belongDomain 所属ドメイン
	 * @return 該当ユーザリスト（EIMUserのList）
	 * @throws Exception
	 */
	public 	static List<EIMUser> getUserFromBelongDomain(EIMSession sess, BelongDomain belongDomain) throws Exception
	{
		List<EIMUser> result = new ArrayList<EIMUser>();

		long id = belongDomain.getBelonging().getId();
		String belongType = belongDomain.getBelongType().getSymbol();
		
		if (belongType.equals(BelongType.USER.getSymbol())) {
			result.add(UserUtils.getUserById(sess, id));
		}
		else if (belongType.equals(BelongType.GROUP.getSymbol())) {
			result.addAll(GroupUtils.getUserListRecurrently(sess, GroupUtils.getGroupById(sess, id)));
		}
		else if (belongType.equals(BelongType.ROLE.getSymbol())) {
			result.addAll(RoleUtils.getUserList(sess, RoleUtils.getRoleById(sess, id)));
		}
		else if (belongType.equals(BelongType.COMPGROUP.getSymbol())) {
			result.addAll(CompUtils.getUserList(sess, CompUtils.getCompById(sess, id)));
		}
		
		return(result);
	}

	/**
	 * 「承認者種別:ID」の形式の文字列から該当ユーザorグループ名を取得して返します
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param code 「承認者種別:ID」の形式の文字列
	 * @return 該当ユーザリスト（EIMUserのList）
	 * @throws Exception
	 */
	public 	static String getNameFromCode(EIMSession sess, String code) throws Exception
	{
		String result = "";
		
		long id = Long.parseLong(code.substring(2));
		switch (Integer.parseInt(code.substring(0, 1)))
		{
		case 1:
			EIMUser user = UserUtils.getUserById(sess, id);
			// 有効ユーザかチェック
			if(user.getDisable() == 0){
				result = UserUtils.getUserById(sess, id).getName();
			}
			break;
		case 2:
			result = GroupUtils.getGroupById(sess, id).getName();
			break;
		case 3:
			result = RoleUtils.getRoleById(sess, id).getName();
			break;
		case 4:
			result = CompUtils.getCompById(sess, id).getName();
			break;		
		}
		return(result);
	}

	/**
	 * 「承認者種別:ID」のリストから該当ユーザリストを取得して返します
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param CSVList 「承認者種別:ID」の形式の文字列のリスト
	 * @return 該当ユーザリスト（EIMUserのList）
	 * @throws Exception
	 */
	public static 	List getUserFromCSV(EIMSession sess, List CSVList) throws Exception
	{
		List result = new ArrayList();

		for(int i = 0; i < CSVList.size(); i++)
		{
			result.addAll(getUserFromCode(sess, String.valueOf(CSVList.get(i))));
		}
		return(result);
	}
	
	/**
	 * 「承認者種別:ID」のリストをグループ→ユーザ、名称順でソートして返します
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param CSVList 「承認者種別:ID」の形式の文字列のリスト
	 * @return ソート後のCSVList
	 * @throws Exception
	 */
	public static List sortCSVList(EIMSession sess, List CSVList) throws Exception
	{
		List result = new ArrayList();
		List groupList = new ArrayList();
		List roleList = new ArrayList();
		List compList = new ArrayList();
		List userList = new ArrayList();
		
		for (int i = 0 ; i < CSVList.size(); i++)
		{
			String kind = ((String)CSVList.get(i)).substring(0, 1);
			int code = Integer.parseInt(((String)CSVList.get(i)).substring(2));
			
			if (kind.equals("1"))
				userList.add(UserUtils.getUserById(sess, code));
			else if (kind.equals("2"))
				groupList.add(GroupUtils.getGroupById(sess, code));
			else if (kind.equals("3"))
				roleList.add(RoleUtils.getRoleById(sess, code));
			else if (kind.equals("4"))
				compList.add(CompUtils.getCompById(sess, code));
		}

		groupList = AppObjectUtil.getStrSortedList(groupList, "getName", true);
		roleList = AppObjectUtil.getStrSortedList(roleList, "getName", true);
		compList = AppObjectUtil.getStrSortedList(compList, "getName", true);
		userList = AppObjectUtil.getStrSortedList(userList, "getName", true);

		for (int i = 0 ; i < groupList.size(); i++)
		{
			result.add("2:" + String.valueOf(((EIMGroup)groupList.get(i)).getId()));
		}
		for (int i = 0 ; i < roleList.size(); i++)
		{
			result.add("3:" + String.valueOf(((EIMRole)roleList.get(i)).getId()));
		}
		for (int i = 0 ; i < compList.size(); i++)
		{
			result.add("4:" + String.valueOf(((EIMComp)compList.get(i)).getId()));
		}
		for (int i = 0 ; i < userList.size(); i++)
		{
			result.add("1:" + String.valueOf(((EIMUser)userList.get(i)).getId()));
		}
		return(result);
	}
	
	/**
	 * 被承認依頼者ユーザの一覧を取得します。
	 * 
	 * <li>重複ユーザは除去して取得します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param statusId ステータスID
	 * @return 被承認依頼者ユーザの一覧
	 * @throws Exception
	 */
	public static 	List getUserListForRequestTo(EIMSession sess, String statusId) throws Exception
	{
		List userList = new ArrayList();
		HashSet set = new HashSet();
		
		// 被承認依頼者オブジェクトの取得
		List reqToList = AppObjectUtil.getObjectList(sess, EIMConfig.get("OBJECT_TYPE_NAME_REQUEST_TO"), statusId);
		
		for (int i = 0 ; reqToList.size() > i ; i++) {
			EIMObject reqToObj = (EIMObject)reqToList.get(i);
			// 属性「被承認依頼者種別：ID」の取得
			String code = AppObjectUtil.getStrAttr(sess, reqToObj, EIMConfig.get("ATTR_NAME_REQUEST_TO_KIND_ID"));
			if (!StringUtils.isBlank(code)) {
				List tmpList = getUserFromCode(sess, code);
				for (int j = 0 ; tmpList.size() > j ; j++) {
					EIMUser user = (EIMUser)tmpList.get(j);
					// ユーザが重複して取得されるためサマリ
					if (set.add(new Long(user.getId()))) {
						userList.add(user);
					}
				}
			}
		}
		return userList;
	}

	/**
	 * 
	 * ログインユーザのタスクリストを取得します。
	 * @param sess セッション
	 * @return タスクリスト
	 * @throws Exception
	 */
	public static List<EIMObject> getApproveRequestedTaskList(EIMSession sess, long userId) throws Exception
	{
		List<EIMObject> objectList = new ArrayList<EIMObject>();
		
		try
		{
			// 暫定対応：アサインされているタスクを取得する
			List<TaskDomain> taskDomainListtmp = getListByEntryId(sess, userId);
			
			if (!taskDomainListtmp.isEmpty())
			{
				// オブジェクトのセキュリティチェック
				/* 2010/11/15 ここで取得するタスクについては、ステータス変更権限有無のチェックは行わない。
				 * userAにアサイン→userAのステータス変更権削除→userAタスク一覧取得
				 * 上記のフローでもアサインされたタスクを全て表示させる必要があるため。
				 */
				Map fldObjTypeIdMap = AppObjectTypeUtil.getObjTypeMap(sess, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));
				Map docObjTypeIdMap = AppObjectTypeUtil.getObjTypeMap(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
				
				EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
				
				EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
				
				Set<Long> objectIdSet = new HashSet<Long>(taskDomainListtmp.size());
				for (TaskDomain task : taskDomainListtmp)
				{
					objectIdSet.add((long) task.getObject().getId());
				}
				
				// ドキュメント管理で使用するオブジェクトタイプのみ対象とする
				//  (ProMo統合にて別オブジェクトタイプが混在するため)
				Set<Long> objectTypeIdSet = new HashSet<Long>(fldObjTypeIdMap.size() + docObjTypeIdMap.size());
				objectTypeIdSet.addAll(fldObjTypeIdMap.keySet());
				objectTypeIdSet.addAll(docObjTypeIdMap.keySet());
				
				selectTarget.setCondition(h.group(h.opAnd())
						.addCondition(h.inArray(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID, h.opIn(), objectIdSet.toArray()))
						.addCondition(h.inArray(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, h.opIn(), objectTypeIdSet.toArray())));
				selectTarget.setResultAttrs(new ArrayList<EIMAttributeType>());
				selectTarget.setRole(-1);
				
				List<?> searchObjects = SearchUtils.searchObjects(sess, selectTarget, new EIMSearchLimitCountCondition(-2, false));
				for (Iterator<?> it = searchObjects.iterator(); it.hasNext(); )
				{
					objectList.add((EIMObject) it.next());
				}
			}
		}
		catch(EIMException eime)
		{
			throw eime;
		}
		catch(Exception e)
		{
			throw e;
		}
		return AppSearchUtils.searchObjectsByConditionMaker(sess, 
				EIMDocSearchType.DISPLAY_APPROVEITEM, EIMAccessRole.READ, null, objectList);
	}

	/**
	 * 指定したユーザが指定したオブジェクトのエントリリストに含まれているかどうかを返します
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param user 対象ユーザ
	 * @param object 対象オブジェクト
	 * @return エントリリストに含まれていればtrue
	 * @throws Exception
	 */
	public 	static boolean isUserInEntry(EIMSession sess, EIMUser user, EIMObject object) throws Exception
	{
		List entryList = SecurityUtils.getAccessEntryList(sess, (EIMSecurity)object.getStatus().getType());
		for (int i = 0 ; i < entryList.size() ; i++)
		{
			List userList = new ArrayList();
			EIMAccessEntry entry = (EIMAccessEntry)entryList.get(i);
			
			if (entry.getType().getId() == EIMAccessEntryType.USER)
			{
				userList.add(entry.getUser());
			}
			else if(entry.getType().getId() == EIMAccessEntryType.GROUP)
			{
				userList = GroupUtils.getUserListRecurrently(sess, entry.getGroup());
			}
			else if(entry.getType().getId() == EIMAccessEntryType.ROLE)
			{
				userList = RoleUtils.getUserList(sess, entry.getRole());
			}
			else if(entry.getType().getId() == EIMAccessEntryType.COMP)
			{
				userList = CompUtils.getUserList(sess, entry.getComp());
			}

			for (int j = 0 ; j < userList.size() ; j++)
			{
				if (user.getId() == ((EIMUser)userList.get(j)).getId()) return(true);
			}
		}
		return(false);
	}

	/**
	 * 指定したユーザリストから、指定したオブジェクトの「ステータス変更」権限を持たないユーザを削除して返す
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param userList 対象ユーザリスト
	 * @param object 対象オブジェクト
	 * @return ユーザ削除後のリスト
	 * @throws Exception
	 */
	public 	static List getWritableUserList(EIMSession sess, List userList, EIMObject object) throws Exception
	{
		int i = 0;
		while (i < userList.size())
		{
			// ステータス変更権限を持つユーザはSTATUS_UPとSTATUS_DOWNの両方を保持するので、ここではSTATUS_UPのみを判定
			if (SecurityUtils.authorized(sess, object, (EIMUser)userList.get(i), EIMAccessRole.STATUS_UP))
				i++;
			else
				userList.remove(i);
		}
		return(userList);
	}
	
	
	/**
	 * オブジェクトが公開処理（PDF変換／PDF分割／PDF署名/URL挿入）を必要とするか否かを返す
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @return 公開処理が必要な場合、trueを返却
	 * @throws Exception
	 */
	public static boolean isNeedAsyncProcess(EIMSession sess, EIMObject object, 
					GuardConditionExecDomain guardConditionExecDomain) throws Exception
	{
		// アドオンクラスのインスタンスリストを取得
		List classList = PublishAddonUtils.getAddonClasses(sess);
		for(int i = 0; i < classList.size() ; i++) {
			PublishCommandAddOn addon = (PublishCommandAddOn)classList.get(i);
			
			boolean ret = false;
			if(guardConditionExecDomain != null)
			{
				ret = addon.isNeedAsyncProcess(sess, object, guardConditionExecDomain);
			}
			else
			{
				ret = addon.isNeedAsyncProcess(sess, object);
			}
			
			// 公開処理実施
			if( ret == true )
			{
				return true;
			}
		}
		return false;
	}
	
	/**
	 * オンライン公開処理実施（バッチ処理を行わないドキュメント用）
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @throws Exception
	 */
	public static void doPublishCommandOnlineNonConvert(EIMSession sess, EIMObject object) throws Exception
	{
		// アドオンクラスのインスタンスリストを取得
		List classList = PublishAddonUtils.getAddonClasses(sess);
		for(int i = 0; i < classList.size() ; i++) {
			PublishCommandAddOn addon = (PublishCommandAddOn)classList.get(i);
			
			addon.doPublishCommandOnline(sess, object);
		}
		
	}
	
	/**
	 * オンライン公開処理実施
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @return 非同期終了の場合、trueを返却
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public static boolean doPublishCommandOnline(EIMSession sess, EIMObject object) throws Exception
	{
		boolean hasAsyncProcess = false;
		
		// 公開処理失敗属性
		String attPublicProcFail = EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL");
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, attPublicProcFail);

		// 子階層取得(WF付フォルダ公開の場合)
		List objectList = AppObjectUtil.getChildEIMObjectRecurrently(sess, object, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		objectList.add(object);
		
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		for (int i = 0; i < objectList.size(); i++)
		{
			EIMObject targetObj = (EIMObject)objectList.get(i);
			EIMObjectType targetObjType = ObjectUtils.getObjectTypeById(sess, targetObj.getType().getId());

			// 以下を満たす場合、公開処理失敗属性をフラグOFFで作成する
			//  1. 処理対象オブジェクト、または、フォルダ以外 
			//  2. 公開処理失敗属性を持っていない
			if ((object.getId() == targetObj.getId() || !helper.isTypeOfFolder(targetObjType))
				&& (targetObj.getAttribute(attPublicProcFail) == null))
			{
				ObjectAttributeUtils.setAttribute(sess, targetObj, attType, 0);
			}
		}

		// アドオンクラスのインスタンスリストを取得
		List classList = PublishAddonUtils.getAddonClasses(sess);
		for(int i = 0; i < classList.size() ; i++){
			PublishCommandAddOn addon = (PublishCommandAddOn)classList.get(i);
			
			// ※ 承認不要WFについて　2010/09/30記
			//		本来なら、V41にて承認不要WFのPDF署名判定のためのガード条件判定用に追加したメソッド
			//		isNeedAsyncProcess(sess, object,guardConditionExecDomain)
			//		を使用するところですが、V41リリース直前のため大きな変更を避けます。
			//		ここでの判定と処理は、承認不要WFのPDF署名では無関係の処理なので
			//		特に問題はありません。
			
			// 公開処理実施
			if (addon.isNeedAsyncProcess(sess, object))
			{
				hasAsyncProcess = true;
				addon.doAsyncProcess(sess, object);
			}
			else
			{
				addon.doPublishCommandOnline(sess, object);
			}
		}
		
		return hasAsyncProcess;

	}
	
	/**
	 * バッチ公開処理実施
	 * @param sess セッション情報
	 * @param object 公開対象オブジェクト
	 * @return 正常終了:true、異常終了:false
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public static boolean doPublishCommandBatch(EIMSession sess,  EIMObject object) throws Exception
	{
		// アドオンクラスのインスタンスリストを取得
		List classList = PublishAddonUtils.getAddonClasses(sess);
		for(int i = 0; i < classList.size() ; i++){
			PublishCommandAddOn addon = (PublishCommandAddOn)classList.get(i);
			
			// 公開処理実施
			boolean isSuccess = addon.doPublishCommandBatch(sess, object);
			
			if (!isSuccess)
				return false;

		}
		return true;
	}
	
	/**
	 * アサインされているオブジェクトを取得する。
	 * TaskDao#getListByEntryIdの処理を移植。
	 * ただし、本処理はオブジェクトIDのみセットしたObjectDomainを生成してTaskDomainにセットしている。
	 * @param sess セッション情報
	 * @param entryId ユーザーID
	 * @return タスクドメイン
	 * @throws Exception
	 */
	public static List<TaskDomain> getListByEntryId(EIMSession sess, long entryId) throws Exception{
		try {
			// スレッドローカルのEIMSession変数を入れ替える
			EIMThreadContext.pushEIMSession(sess);

			// 処理委譲する
			ApplicationContext context = ApplicationContextLoader.getContext();
			TaskService taskService = (TaskService)context.getBean("taskService");
			return taskService.getListByEntryId(entryId);
		}
		finally {
			// スレッドローカルのEIMSession変数を復帰させる
			EIMThreadContext.popEIMSession();
		}
	}
	
}