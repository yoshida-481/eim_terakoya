<%@page import="java.util.Map.Entry"%>
<%@page import="eim.bo.EIMAccessRole.PermitMode"%>
<%@page import="eim.bo.EIMAccessRole.PermitMode"%>
<%@page import="eim.bo.EIMAccessRole.PermitMode"%>
<%@page import="eim.bo.EIMAccessRole.PermitMode"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.EntryDomain"%>
<%@page import="jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "java.sql.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "org.springframework.context.ApplicationContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.dao.AssignEntryDao"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.service.UserDefGroupConfService"%>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.BelongType"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.UserDefGroupDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.BelongDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.AssignEntryDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.service.UserDefGroupPlugIn"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain"%>
<%@ page import="jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page import="jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@ page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@ page import="jp.co.ctc_g.eim.framework2.business.domain.criteria.UserCriteria"%>
<%@ page import="jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria"%>
<%@ page import="jp.co.ctc_g.eim.framework2.business.service.UserService"%>
<%@ page import="jp.co.ctc_g.eim.framework2.business.domain.entity.*"%>

<%
	
	/**
	 * エントリ情報の基底クラス
	 */
	abstract class Entry
	{
		protected EntryDomain entryDomain;
		
		/**
		 * コンストラクタ
		 */
		public Entry()
		{
		}
		public Entry(EntryDomain entryDomain)
		{
			this.entryDomain = entryDomain;
		}

		/**
		 * エントリのIDを返却する
		 * @return エントリID
		 */
		public long getId()
		{
			return entryDomain.getId();
		}
		
		/**
		 * エントリの名称を返却する
		 * @return エントリ名称
		 */
		public String getName()
		{
			return entryDomain.getName();
		}

		abstract public List<EIMUser> getUserList(EIMSession sess) throws Exception;

	}

	/**
	 * ユーザエントリ情報のクラス
	 */
	class UserEntry extends Entry
	{
		private EIMUser user = null;
		private HashMap<Long, String> groupMap = new HashMap<Long, String>();
		private HashMap<Long,String> roleMap = new HashMap<Long, String>();
		private boolean bossFlag = false;

		/**
		 * コンストラクタ
		 */
		public UserEntry(EIMUser user)
		{
			this.user = user;
			
			entryDomain = new EntryDomain();
			entryDomain.setId(user.getId());
			entryDomain.setName(user.getName());
		}
		

		/**
		 * エントリに所属する有効なユーザ情報を返却する
		 * @param sess セッション情報
		 * @return エントリに所属するユーザ情報
		 */
		public List<EIMUser> getUserList(EIMSession sess) throws Exception
		{
			List<EIMUser> users = new ArrayList<EIMUser>();
			
			return users;
		}
		
		/**
		 * エントリに設定されているEIMユーザ情報を返却する
		 * @return EIMユーザ情報
		 */
		public EIMUser getEIMUser(){
			return this.user;
		}
		
		/**
		* ユーザがエントリされているグループ名をMapに格納する
		* @param id グループID
		* @param name グループ名
		*/
		public void setGroupMap(long id, String name){
			if(!groupMap.containsKey(id)){
				groupMap.put(id , name);
			}
		}
		
		/**
		* ユーザがエントリされているグループ名が格納されているMapを返却する
		* @return HashMap<グループID,グループ名> 
		*/
		public HashMap<Long, String> getGroupMap(){
			return this.groupMap;
		}
		
		/**
		* ユーザがエントリされているロール名をMapに格納する
		* @param id ロールID
		* @param name ロール名
		*/
		public void setRoleMap(long id, String name){
			if(!roleMap.containsKey(id)){
				roleMap.put(id , name);
			}
		}
		
		/**
		* ユーザがエントリされているロール名が格納されているMapを返却する
		* @return HashMap<ロールID,ロール名> 
		*/
		public HashMap<Long, String> getRoleMap(){
			return this.roleMap;
		}
		
		/**
		 * ユーザが上長か判定し返却する
		 * @return bossFlag true:上長、false:上長以外
		 */
		public boolean getBoosFlag(){
			return this.bossFlag;
		}
		
		/**
		 * ユーザが上長か判定した結果を設定する
		 * @param bossFlag true:上長、false:上長以外
		 */
		public void setBoosFlag(boolean bossFlag){
			this.bossFlag = bossFlag;
		}
	}

	/**
	 * グループエントリ情報のクラス
	 */
	class GroupEntry extends Entry
	{

		private EIMGroup group = null;
		
		/**
		 * コンストラクタ
		 */
		public GroupEntry(EIMGroup group)
		{
			this.group = group;

			entryDomain = new EntryDomain();
			entryDomain.setId(group.getId());
			entryDomain.setName(group.getName());
		}

		/**
		 * エントリに所属する有効なユーザ情報を返却する
		 * @param sess セッション情報
		 * @return エントリに所属するユーザ情報
		 */
		public List<EIMUser> getUserList(EIMSession sess) throws Exception
		{
			return GroupUtils.getUserListRecurrently(sess, group, 0 ,false);
		}
	}
	
	/**
	 * ロールエントリ情報のクラス
	 */
	class RoleEntry extends Entry
	{
		
		private EIMRole role = null;
		/**
		 * コンストラクタ
		 */
		public RoleEntry(EIMRole role)
		{
			this.role = role;
			
			entryDomain = new EntryDomain();
			entryDomain.setId(role.getId());
			entryDomain.setName(role.getName());
		}

		/**
		 * エントリに所属する有効なユーザ情報を返却する
		 * @param sess セッション情報
		 * @return エントリに所属するユーザ情報
		 */
		public List<EIMUser> getUserList(EIMSession sess) throws Exception
		{
			return RoleUtils.getUserList(sess, role, 0, false);
		}
		
	}
	
	/**
	 * 複合グループエントリ情報のクラス
	 */
	class CompEntry extends Entry
	{
		private EIMComp comp = null;

		/**
		 * コンストラクタ
		 */
		public CompEntry(EIMComp comp)
		{
			this.comp = comp;

			entryDomain = new EntryDomain();
			entryDomain.setId(comp.getId());
			entryDomain.setName(comp.getName());
		}

		/**
		 * エントリに所属する有効なユーザ情報を返却する
		 * @param sess セッション情報
		 * @return エントリに所属するユーザ情報
		 */
		public List<EIMUser> getUserList(EIMSession sess) throws Exception
		{
			return CompUtils.getUserList(sess, comp, 0);
		}
		
		/**
		 * エントリから複合グループを取得する
		 * @return 複合グループ
		 */
		public EIMComp getEIMcomp(){
			return this.comp;
		}
	}
	
	/**
	 * ユーザ定義グループエントリ情報のクラス
	 */
	class UserDefGroupEntry extends Entry
	{
		private UserDefGroupDomain userDefGroupDomain = null;
		private EIMObject object = null;
		
		/**
		 * コンストラクタ
		 */
		public UserDefGroupEntry(UserDefGroupDomain userDefGroupDomain, EIMObject object)
		{
			this.userDefGroupDomain = userDefGroupDomain;
			
			entryDomain = new EntryDomain();
			entryDomain.setId(userDefGroupDomain.getId());
			entryDomain.setName(userDefGroupDomain.getName());

			this.object = object;
		}

		/**
		 * エントリに所属する有効なユーザ情報を返却する
		 * @param sess セッション情報
		 * @return エントリに所属するユーザ情報
		 */
		public List<EIMUser> getUserList(EIMSession sess) throws Exception
		{
			ApplicationContext context = ApplicationContextLoader.getContext();

			String key = userDefGroupDomain.getKey();
			UserDefGroupPlugIn userGroup = (UserDefGroupPlugIn)context.getBean(key);
			ObjectDomain objectDomain = new ObjectDomain(object);
			List<UserDomain> userDomainList = userGroup.getUserListByObject(objectDomain);

			List<EIMUser> userList = new ArrayList<EIMUser>();
			for (UserDomain userDomain : userDomainList)
			{
				if (userDomain.getDisable() == 0)
				{
					//有効ユーザの場合
					userList.add(userDomain.createEIMUser());
				}
			}
			
			return userList;
		}
	}
	/**
	 * エントリ毎の所属ユーザをキャッシュするクラス
	 */
	class EntryUserListCache
	{

		// Map＜エントリID, EIMUser＞
		// エントリIDごとに所属するユーザ情報を保持する
		private Map<Long, List<EIMUser>> entryIdAndUserListMap
				= new HashMap<Long, List<EIMUser>>();

		// Map＜エントリID, Set＜ユーザID＞＞
		// エントリIDごとに所属するユーザIDのSetを保持する
		// エントリ内に該当ユーザが存在するかどうかチェックするために使用
		private Map<Long, Set<Long>> entryIdAndUserSet 
				= new HashMap<Long, Set<Long>>();

		/**
		 * キャッシュ済みのエントリの所属ユーザリストを返却する。
		 * キャッシュされていなければ、取得し重複を除いたユーザリストを返却する。
		 *
		 * @param sess セッション
		 * @param entry エントリ情報
		 * @return エントリの所属ユーザリスト
		 */
		public List<EIMUser> getUserList(EIMSession sess, Entry entry) throws Exception
		{
			List<EIMUser> userList = entryIdAndUserListMap.get(entry.getId());
			if (userList != null)
			{
				// キャッシュ済み
				return userList;
			}
			userList = deleteDuplicateUser(entry.getUserList(sess));
			
			entryIdAndUserListMap.put(entry.getId(), userList);
			
			return userList;
		}

		/**
		 * ユーザがエントリに所属しているかどうかを返却する
		 * @param sess セッション
		 * @param entry エントリ情報
		 * @param user ユーザ情報
		 * @return ユーザがエントリに所属しているかどうか
		 */
		public boolean containsUser(EIMSession sess, Entry entry, EIMUser user) throws Exception
		{
			Set<Long> userSet = entryIdAndUserSet.get(entry.getId());
			if (userSet == null)
			{
				userSet = new HashSet<Long>();
				List<EIMUser> userList = getUserList(sess, entry);
				
				for (EIMUser entryUser : userList)
				{
					userSet.add((long)entryUser.getId());
				}
				
				entryIdAndUserSet.put(entry.getId(), userSet);
			}

			if (!userSet.contains((long)user.getId()))
			{
				return false;
			}

			return true;
		}
		
		/**
		 * ユーザ情報リストから重複したユーザ情報リストを削除する
		 * グループエントリで親、子に同じユーザが存在する場合の対処
		 * @param userList ユーザ情報リスト
		 * @return 重複を削除したユーザ情報リスト
		 */
		private List<EIMUser> deleteDuplicateUser(List<EIMUser> userList)
		{
			Set<Long> userIdSet = new HashSet<Long>();
			List<EIMUser> retUserList = new ArrayList<EIMUser>();
			for (EIMUser user : userList)
			{
				if (!userIdSet.contains((long)user.getId()))
				{
					retUserList.add(user);
					userIdSet.add((long)user.getId());
				}
			}
			
			return retUserList;
		}
	}
	
	/**
	 * アサイン先エントリ情報のクラス
	 */
	class AssignEntry
	{
		private EntryUserListCache entryUserListCache = null;
		private Entry entry = null;
		
		/**
		 * コンストラクタ
		 */
		public AssignEntry(EntryUserListCache entryUserListCache, Entry entry)
		{
			this.entryUserListCache = entryUserListCache;
			this.entry = entry;
		}

		/**
		 * エントリ情報を返却する
		 * @return エントリ情報
		 */
		public Entry getEntry()
		{
			return entry;
		}

		/**
		 * エントリの所属ユーザリストを返却する。
		 *
		 * @param sess セッション
		 * @return エントリの所属ユーザリスト
		 */
		public List<EIMUser> getUserList(EIMSession sess) throws Exception
		{
			return entryUserListCache.getUserList(sess, entry);
		}
		
	}

	/**
	 * アクセスエントリ情報のクラス
	 */
	class AccessEntry
	{
		private EntryUserListCache entryUserListCache = null;
		private Entry entry = null;
		
		/**
		 * コンストラクタ
		 */
		public AccessEntry(EntryUserListCache entryUserListCache, Entry entry)
		{
			this.entryUserListCache = entryUserListCache;
			this.entry = entry;
		}

		/**
		 * エントリの所属ユーザリストを返却する。
		 *
		 * @param sess セッション
		 * @return エントリの所属ユーザリスト
		 */
		public List<EIMUser> getUserList(EIMSession sess) throws Exception
		{
			return entryUserListCache.getUserList(sess, entry);
		}
		
		/**
		 * ユーザがエントリに所属しているかどうかを返却する
		 * @param sess セッション
		 * @param user ユーザ情報
		 * @return ユーザがエントリに所属しているかどうか
		 */
		public boolean containsUser(EIMSession sess, EIMUser user) throws Exception
		{
			return entryUserListCache.containsUser(sess, entry, user);
		}
	}

	/**
	 * アクセス権限のクラス
	 */
	class AccessRole
	{
		private EntryUserListCache entryUserListCache = null;

		private Boolean isUseAuthorizedMethod = null;
		
		private boolean existsUserDefGroup = false;	// アクセスエントリにユーザ定義グループが存在するか
		private List<EIMAccessEntry> accEntryList = null;
		private Map<AccessEntry, EIMAccessRole.PermitMode> accessEntryAndPermitModeMap = null;

		/**
		 * コンストラクタ
		 */
		public AccessRole(EntryUserListCache entryUserListCache)
		{
			this.entryUserListCache = entryUserListCache;
		}
		
		/**
		 * ユーザが権限を保持しているかどうかを返却する
		 * @param sess セッション
		 * @param object チェック対象オブジェクト情報
		 * @param user チェック対象ユーザ情報
		 * @param accessRoleId チェック対象アクセス権限
		 * @return ユーザが権限を保持しているか
		 */
		public boolean isAuthorize(EIMSession sess, EIMObject object, EIMUser user, int accessRoleId) throws Exception
		{
			if (isUseAuthorizedMethod())
			{
				return SecurityUtils.authorized(
							sess, object, user, accessRoleId);
			}

			init(sess, object, accessRoleId);

			if (!existsUserDefGroup)
			{
				// ユーザ定義グループがアクセスエントリに設定されていない場合は、
				// 既存のメソッド（プロシージャにてACUから検索）を使用
				return SecurityUtils.enabled(
							sess, object, user, accessRoleId);
			}
			
			return enabled(
					sess, object.getSecurity(), user, accessRoleId);
		}

		/**
		 * authorizedメソッドを使用するかどうか且つアクセスエントリリスト内にユーザ定義グループが存在するかどうか判定する
		 * @param sess セッション
		 * @param object チェック対象オブジェクト情報
		 * @return authorized()メソッドを使用しない且つアクセスエントリリスト内にユーザ定義グループが存在しない場合true
		 */
		public boolean checkIsUseAuthorizedMethodAndExistsUserDefGroup(EIMSession sess, EIMObject object) throws Exception
		{
			if (isUseAuthorizedMethod())
			{
				return false;
			}

			init(sess, object, EIMAccessRole.STATUS_UP);

			if (!existsUserDefGroup) {
				return true;
			} else {
				return false;
			}
		}

		/**
		 * 権限チェックの際enable()を使用する際の初期化処理
		 * 無視以外のアクセス権限一覧を取得する
		 *
		 * @param sess セッション
		 * @param object チェック対象オブジェクト情報
		 * @param accessRoleId チェック対象アクセス権限
		 */
		private void init(EIMSession sess, EIMObject object, int accessRoleId) throws Exception
		{
			if (accEntryList != null)
			{
				// 初期化済みなら何もしない
				return;
			}

			// 変数初期化
			accessEntryAndPermitModeMap = new LinkedHashMap<AccessEntry, EIMAccessRole.PermitMode>();

			EIMSecurity sec = object.getSecurity();
			accEntryList = SecurityUtils.getAccessEntryList(sess, sec);

			existsUserDefGroup = existsUserDefGroup(accEntryList);
			if (!existsUserDefGroup)
			{
				// ユーザ定義グループがアクセスエントリに含まれない場合
				// 以下の処理は不要
				// 以下の処理は、enable()のユーザ定義グループ対応のために必要
				return;
			}
			
			// accessEntryAndPermitModeMapを初期化しつつ、ユーザ定義グループが存在するかチェックする
			for (EIMAccessEntry accEntry : accEntryList)
			{
				Entry entry = getEntry(accEntry, object);
				
				// ステータス別セキュリティ
				EIMStatusSecurity statusSec 
						= StatusSecurityUtils.getStatusSecurityBySecurityAndStatusType(
								sess, object.getSecurity(), object.getStatus().getType() );

				// デフォルトセキュリティ or ステータス別セキュリティ取得
				List<EIMAccessRole> acrList = SecurityUtils.getAccessRoleList(sess, accEntry, statusSec);
				for (EIMAccessRole acr : acrList)
				{
					if (acr.getId() == accessRoleId
							&& !acr.getPermit().equals(EIMAccessRole.PermitMode.IGNORE))
					{
						AccessEntry accessEntry 
								= new AccessEntry(entryUserListCache, entry);

						accessEntryAndPermitModeMap.put(accessEntry, acr.getPermit());
						
						if (entry instanceof UserDefGroupEntry)
						{
							// ユーザ定義グループがチェック対象の
							// アクセスエントリに含まれる場合
							existsUserDefGroup = true;
						}
					}
				}
			}
		}

		/**
		 * ユーザが権限を保持しているかどうかを返却する
		 * enabled()のユーザ定義グループ対応版
		 * 指定ユーザが各アクセスエントリに所属するかをJava側で判定する
		 * 
		 * @param sess セッション
		 * @param security セキュリティ情報
		 * @param user チェック対象ユーザ情報
		 * @param accessRole アクセス権限のID
		 * @return ユーザが権限を保持しているか
		 */
		private boolean enabled(
				EIMSession sess, EIMSecurity security, EIMUser user, int accessRole) throws Exception
		{
			for (Map.Entry<AccessEntry, EIMAccessRole.PermitMode> mapEntry 
					: (accessEntryAndPermitModeMap.entrySet()))
			{
				AccessEntry accessEntry = mapEntry.getKey();
				
				// 許可/拒否を取得（無視は含まれない）
				EIMAccessRole.PermitMode permitMode 
					= accessEntryAndPermitModeMap.get(accessEntry);
				
				if (!accessEntry.containsUser(sess, user))
				{
					continue;
				}

				if (permitMode == permitMode.ACCEPT)
				{
					// 許可
					return true;
				}

				// 拒否/削除（削除は、QueryBuilderAndLoaderEIMOBJのSQLにあわせて拒否扱いとする）
				return false;
			}
			// 権限なし
			return false;
		}

		/**
		 * アクセスエントリ情報を取得する
		 *
		 * @param accEntry アクセスエントリ情報（EIM標準）
		 * @param object チェック対象オブジェクト情報
		 */
		private Entry getEntry(EIMAccessEntry accEntry, EIMObject object)
		{
			Entry entry = null;

			int accessEntryTypeId = accEntry.getType().getId();
			switch (accessEntryTypeId) 
			{
				case EIMAccessEntryType.USER:
					entry = new UserEntry(accEntry.getUser());
					break;

				case EIMAccessEntryType.GROUP:
					entry = new GroupEntry(accEntry.getGroup());
					break;

				case EIMAccessEntryType.ROLE:
					entry = new RoleEntry(accEntry.getRole());
					break;

				case EIMAccessEntryType.COMP:
					entry = new CompEntry(accEntry.getComp());
					break;

				case EIMAccessEntryType.USERDEF:
					entry = new UserDefGroupEntry(accEntry.getUserDefGroup(), object);
					break;
			}
			
			return entry;
		}

		/**
		 * authorized()メソッドを使用するかどうかを返却する
		 * configファイルの設定値から判定する
		 *
		 * @return authorized()メソッドを使用するかどうか
		 */
		private boolean isUseAuthorizedMethod()
		{
			if (isUseAuthorizedMethod != null)
			{
				return isUseAuthorizedMethod.booleanValue();
			}
			
			int authorizedMethodUseFlg = 
					Integer.parseInt(EIMConfig.get("DSPAPPROVER_AUTHORIZE_METHOD_USE_FLAG"));

			isUseAuthorizedMethod = (authorizedMethodUseFlg == AppConstant.AUTHORIZE_METHOD_USE_FLAG_ON);
			
			return isUseAuthorizedMethod.booleanValue();
		}
		
		/**
		 * アクセスエントリリスト内にユーザ定義グループが存在するかどうかを返却する
		 *
		 * @param accEntryList アクセスエントリ(標準)リスト
		 * @return ユーザ定義グループが存在するかどうか
		 */
		private boolean existsUserDefGroup(List<EIMAccessEntry> accEntryList)
		{
			for (EIMAccessEntry accEntry : accEntryList)
			{
				int accessEntryTypeId = accEntry.getType().getId();
				if (accessEntryTypeId == EIMAccessEntryType.USERDEF)
				{
					return true;
				}
			}
			return false;
		}
	}

	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());
	
	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;
	
	//Parameter
	String prmObjId = request.getParameter("objId");
	String prmStatusTypeId = request.getParameter("statusTypeId");
	String prmOnlyBossInfo = request.getParameter("selectedCheckBox");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"statusTypeId=" + prmStatusTypeId
			};
	
	// val
	boolean sessPutflg = false;
	CallableStatement stmt = null;
	ResultSet rset = null;
	
	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		sessPutflg = false;
		if(EIMThreadContext.getEIMSession() == null)
		{
			//Service、Dao呼出に必要
			EIMThreadContext.putEIMSession(sess);
			sessPutflg = true;
		}
		loginUser = (EIMUser)sess.getAttribute("USER");
		
		// TransactionContextの作成、設定
		if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
		{
			jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
		}
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));

		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}
		EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByStatusType(sess, object.getStatus().getType());
		EIMObject wfSettingObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), "" + workflow.getId());
		
		// 上長のみ表示デフォルト設定有無を取得
		boolean onlyBoss = AppWorkFlowUtil.checkDefaultBossOnly(wfSettingObj , Long.parseLong(prmStatusTypeId));
				
		BossGroupUtils bossGroups = new BossGroupUtils(sess, loginUser);
		BossRoleUtils bossRoles = new BossRoleUtils(sess, loginUser);

		EntryUserListCache entryUserCache = new EntryUserListCache();
		AccessRole accessRole = new AccessRole(entryUserCache);
		
		List<EIMUser> userList = new ArrayList<EIMUser>();
		List<AssignEntry> groupList = new ArrayList<AssignEntry>();
		List<AssignEntry> roleList = new ArrayList<AssignEntry>();
		List<AssignEntry> compList = new ArrayList<AssignEntry>();
		List<AssignEntry> userDefGroupList = new ArrayList<AssignEntry>();
		
		//次ステータスのエントリリストを全て取得して処理
		AssignEntryDao assignEntryDao = (AssignEntryDao)ApplicationContextLoader.getContext().getBean("assignEntryDao");
		AssignEntryDomain assignEntryDomain = new AssignEntryDomain();
		assignEntryDomain.setSttid(Long.parseLong(prmStatusTypeId));
		List<BelongDomain> belongDomainList = (List<BelongDomain>)assignEntryDao.getList(assignEntryDomain);

		for (int i = 0 ; i < belongDomainList.size() ; i++)
		{
			BelongDomain belongDomain = belongDomainList.get(i);

			//ユーザ
			if(belongDomain.getBelongType() == BelongType.USER)
			{
				EIMUser user = UserUtils.getUserById(sess, belongDomain.getBelonging().getId());
				// 現在は、削除されたユーザなどのEIMUserにないENTRYがEIMASENTRYに存在するのでNullチェックを行う
				if( user == null )
				{
					continue;
				}
				if(accessRole.isAuthorize(sess, object, user, EIMAccessRole.STATUS_UP)){
					//有効ユーザの場合
					if (user.getDisable() == 0){
						userList.add(user);
					}
				}
			}
			//グループ
			else if(belongDomain.getBelongType() == BelongType.GROUP)
			{
				EIMGroup group = GroupUtils.getGroupById(sess, belongDomain.getBelonging().getId());
				// 現在は、削除されたグループなどのEIMGroupにないENTRYがEIMASENTRYに存在するのでNullチェックを行う
				if( group == null )
				{
					continue;
				}
				Entry entry = new GroupEntry(group);
				AssignEntry assignEntry = new AssignEntry(entryUserCache, entry);
				
				groupList.add(assignEntry);
				
			}
			//ロール
			else if(belongDomain.getBelongType() == BelongType.ROLE)
			{
				EIMRole role = RoleUtils.getRoleById(sess, belongDomain.getBelonging().getId());
				// 現在は、削除されたロールなどのEIMRoleにないENTRYがEIMASENTRYに存在するのでNullチェックを行う
				if( role == null )
				{
					continue;
				}
				Entry entry = new RoleEntry(role);
				AssignEntry assignEntry = new AssignEntry(entryUserCache, entry);
				
				roleList.add(assignEntry);
				
			}
			//複合グループ
			else if(belongDomain.getBelongType() == BelongType.COMPGROUP)
			{
				EIMComp comp = CompUtils.getCompById(sess, belongDomain.getBelonging().getId());
				// 現在は、削除された複合グループなどのEIMCmpにないENTRYがEIMASENTRYに存在するのでNullチェックを行う
				if( comp == null )
				{
					continue;
				}
				Entry entry = new CompEntry(comp);
				AssignEntry assignEntry = new AssignEntry(entryUserCache, entry);
				
				compList.add(assignEntry);
			}
			// ユーザ定義グループ
			else if(belongDomain.getBelongType() == BelongType.USERDEFGROUP)
			{
				ApplicationContext applicationContext = ApplicationContextLoader.getContext();
				UserDefGroupConfService uds = (UserDefGroupConfService)applicationContext.getBean("UserDefGroupConfService");
				long k = belongDomain.getBelonging().getId();
				UserDefGroupDomain userDefGroupDomain = uds.getUserDefGroup(k);
				if( userDefGroupDomain == null )
				{
					continue;
				}

				Entry entry = new UserDefGroupEntry(userDefGroupDomain, object);
				AssignEntry assignEntry = new AssignEntry(entryUserCache, entry);
				
				userDefGroupList.add(assignEntry);
			}
		}
		
		// ユーザエントリに、グループ、ロール情報を設定
		HashMap<Long,UserEntry> userMap = new HashMap<Long,UserEntry>();
		for (EIMUser user : userList){
			UserEntry userEntry = new UserEntry(user);
			// 上長フラグを設定
			if(bossGroups.isInGroup(GroupUtils.getGroupByUser(sess, user)) && bossRoles.isInRole(RoleUtils.getRoleByUser(sess, user))){
				userEntry.setBoosFlag(true);
			}
			userMap.put((long)user.getId(),userEntry);
		}
		List<AssignEntry> assignEntryList = new ArrayList<AssignEntry>();
		assignEntryList.addAll(groupList);
		assignEntryList.addAll(roleList);
		assignEntryList.addAll(compList);
		assignEntryList.addAll(userDefGroupList);

		//ユーザ以外のエントリを設定
		for (AssignEntry assignEntry :  assignEntryList){
			List<EIMUser> entryUserList = assignEntry.getUserList(sess);

			// SQL条件用にユーザIDを1000件ずつに分割
			List<List<Long>> userIdsList = new ArrayList<>();
			List<Long> userIdList= new ArrayList<>();
			int cnt = 0;
			for(EIMUser user: entryUserList) {
				++cnt;

				userIdList.add(Long.valueOf(user.getId()));

				// IDを1000件ずつに分割
				if(cnt == 1000) {
					cnt = 0;
					userIdsList.add(userIdList);
					userIdList = new ArrayList<>();
				}
			}

			if(userIdList.size() != 0) {
				userIdsList.add(userIdList);
			}

			// 対象ユーザのロールマップを取得します
			Map<Long, List<EIMRole>> userRoleMap = AppWorkFlowUtil.getUserRoleMap(userIdsList);
			// 対象ユーザのグループマップを取得します
			Map<Long, List<EIMGroup>> userGroupMap = AppWorkFlowUtil.getUserGroupMap(userIdsList);

			Set<Long> authorizedUserIdList = new HashSet<>();
			boolean isUseAuthorizedMethodAndExistsUserDefGroup = accessRole.checkIsUseAuthorizedMethodAndExistsUserDefGroup(sess, object);

			if(isUseAuthorizedMethodAndExistsUserDefGroup) {
				authorizedUserIdList = AppWorkFlowUtil.getAuthorizedUserIds(object, userIdsList, EIMAccessRole.STATUS_UP);
			}

			for (EIMUser entryUser : entryUserList){
				// ユーザの権限チェック
				boolean isAuthorize = false;
				if(isUseAuthorizedMethodAndExistsUserDefGroup) {
					if(authorizedUserIdList.contains(Long.valueOf(entryUser.getId()))) {
						isAuthorize = true;
					}
				} else {
					if (accessRole.isAuthorize(sess, object, entryUser, EIMAccessRole.STATUS_UP)){
						isAuthorize = true;
					}
				}

				if(isAuthorize) {
					UserEntry userEntry = new UserEntry(entryUser);
					// 上長かチェック
					if((userRoleMap.containsKey(Long.valueOf(entryUser.getId())) && bossRoles.isInRole(userRoleMap.get(Long.valueOf(entryUser.getId())))) &&
							(userGroupMap.containsKey(Long.valueOf(entryUser.getId())) && bossGroups.isInGroup(userGroupMap.get(Long.valueOf(entryUser.getId()))))){
						userEntry.setBoosFlag(true);
					}
					userMap.put((long)entryUser.getId(),userEntry);
				}
			}
		}
		
		//XML出力開始
		out.println("<approverInfo>");
		
		// 上長のみ表示のON/OFFを設定
		out.println("<onlyBoss flag=\"" + onlyBoss + "\"/>");
		
		out.println("<userList>");

		// 検索条件の設定
		UserCriteria userCriteria = new UserCriteria();
		// 有効ユーザのみ検索対象とする
		userCriteria.setDisable(false);
		// 取得したユーザで絞り込む
		List<UserEntry> outputUserList = new ArrayList<UserEntry>(userMap.values());
		MultipleCriteria<Long> userIds = new MultipleCriteria<Long>();
		for(UserEntry userEntry : outputUserList){
			userIds.add((long)userEntry.getEIMUser().getId());
		}
		userCriteria.setIds(userIds);
		// ユーザ検索の実行
		UserService userService = (UserService)jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader.getApplicationContext().getBean("userServiceForUserSearch");
		List<jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain> eimUserList = userService.getList(userCriteria);

		// 多言語名称がnullなら定義名称をセット
		for (jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain user : eimUserList) {
			if (user.getName() == null) {
				OtherNameDomain otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId(context.getLangId());
				otherNameDomain.setName(user.getDefinitionName());
				user.getNameList().add(otherNameDomain);
			}
			for (GroupDomain group : user.getGroupList()) {
				if (group.getName() == null) {
					OtherNameDomain otherNameDomain = new OtherNameDomain();
					otherNameDomain.setLangId(context.getLangId());
					otherNameDomain.setName(group.getDefinitionName());
					group.getNameList().add(otherNameDomain);
					
				}
			}
			for (RoleDomain role : user.getRoleList()) {
				if (role.getName() == null) {
					OtherNameDomain otherNameDomain = new OtherNameDomain();
					otherNameDomain.setLangId(context.getLangId());
					otherNameDomain.setName(role.getDefinitionName());
					role.getNameList().add(otherNameDomain);
				}
			}
		}

		//ユーザリストをユーザ名称でソート
		eimUserList = AppObjectUtil.getStrSortedList(eimUserList, "getName", true);
		
		for(jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain user : eimUserList){
			UserEntry userEntry = userMap.get((long)user.getId());
			// 所属グループ名称
			String allGroupName = "";
			List belongingGroupList = user.getGroupList();
			if (belongingGroupList != null) {
				allGroupName = UserListUtil.toSeparatedGroupNames(belongingGroupList);
			}
			// 所属ロール名称
			String allRoleName = "";
			List belongingRoleList = user.getRoleList();
			if (belongingRoleList != null) {
				allRoleName = UserListUtil.toSeparatedRoleNames(belongingRoleList);
			}
			
			//XML出力
			out.println("<user");
				out.println(" userId=\"" + user.getId() + "\"");
				out.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
				out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
				out.println(" userKana=\"" + StringUtils.xmlEncode(user.getKana()) + "\"");
				out.println(" groupName=\"" + StringUtils.xmlEncode(allGroupName) + "\"");
				out.println(" roleName=\"" + StringUtils.xmlEncode(allRoleName) + "\"");
				out.println(" bossFlag=\"" + userEntry.getBoosFlag() + "\"");
			out.println(">");
			
			// グループ名を出力する
			out.println("<groupList>");
			for(GroupDomain group : user.getGroupList()){
				String groupName = group.getName() == null ? group.getDefinitionName() : group.getName();
				out.println("<group name=\"" + StringUtils.xmlEncode(groupName) + "\" />");
			}
			
			out.println("</groupList>");
			
			// ロール名を出力する
			out.println("<roleList>");
			for(RoleDomain role : user.getRoleList()){
				String roleName = role.getName() == null ? role.getDefinitionName() : role.getName();
				out.println("<role name=\"" + StringUtils.xmlEncode(roleName) + "\" />");
			}
			
			out.println("</roleList>");
			
			out.println("</user>");
		}
		
		out.println("</userList>");
		out.println("</approverInfo>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	finally
	{
		try{
			if(sessPutflg == true){
				EIMThreadContext.removeEIMSession();
				sessPutflg = false;
			}
			if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
			{
				jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
			}
			if(sess != null) sess.close();
			if(stmt != null){
				stmt.close();
			}
			if(rset != null){
				rset.close();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(se.getMessage()));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	
%>
