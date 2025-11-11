package jp.co.ctc_g.eim.app.document.common.aop.advice;

import java.util.ArrayList;
import java.util.Objects;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppConstant;
import common.util.AppObjectUtil;
import common.util.AppSecurityUtils;
import common.util.WorkSpaceUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMObject;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMXmlConfigAdminAuth;
import eim.util.ObjectUtils;
import eim.util.SecurityUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.ObjectCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.aop.advice.AccessAuthorityCheckAdvice;
import jp.co.ctc_g.eim.framework2.common.aop.plugin.ParameterConverterPlugIn;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;

/**
 * 対象データに対するアクセスが許可されているかチェックするためのアドバイスクラスです。<br>
 * アプリケーションコンテキスト設定における、AOPのアドバイスbeanとして宣言し、
 * サービスファサードへのAOPとして適用してください。
 *
 * @since Ver1.0
 */
public class DocumentAccessAuthorityCheckAdvice extends AccessAuthorityCheckAdvice {

	/**
	 * アクセスロールタイプ定義名称
	 */
	private String accessRoleTypeName;
	/**
	 * アクセス権限が拒否時に発生するエラーのコード
	 */
	private String accessErrorCode;
	/**
	 * 権限チェック対象のオブジェクトが存在しなかった時に発生するエラーのコード
	 */
	private String notFoundErrorCode;
	/**
	 * パラメータコンバータ
	 */
	private ParameterConverterPlugIn parameterConverter;
	/**
	 * 存在チェック、権限チェックに使用するオブジェクトService
	 */
	private ObjectService objectService;

	/**
	 * 開発者がAOPの組み込み情報状態を確認するためのログ
	 */
	private static Log log = LogFactory.getLog(AccessAuthorityCheckAdvice.class);

	/**
	 * 組み込んだ対象のメソッドの特定の引数の値を元にアクセスが許可されているかのチェックを行います。<br>
	 * <br>
	 * 権限チェックを行うためには権限判定対象のオブジェクトの情報(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain)が必要です。<br>
	 *
	 * ParameterConverterPlugInからObjectDomainを取得し、accessRoleTypeNameの定義名称のアクセスロールタイプのアクセス権限判定を行います。<br>
	 * ParameterConverterPlugInはObjectDomainを返却するように実装して下さい。
	 * ParameterConverterPlugInの設定がない場合はメソッドの引数から取得した情報でアクセス権限判定を行います。引数にObjectDomainを指定する必要があります。<br>
	 * いづれの場合も型が不一致の場合はキャストエラーが発生します。<br>
	 * <br>
	 * 権限判定対象のオブジェクトが存在しない、または権限判定対象のオブジェクトへのアクセスが許可されていない場合は、<br>
	 * プロパティに設定されたエラーコードのエラーが発生します。<br>
	 * <br>
	 *
	 * 本メソッドはアプリケーション開発時の補助機能として、ログレベルをDEBUGに設定し、かつ本メソッドが呼び出された場合、<br>
	 * 「本クラス名 + exceuted」の形式でログが出力する機能があります。
	 *
	 * @param target アクセスが許可されているか判定するための元となる情報
	 * @throws Exception 以下の例外を通知します。
	 * <p style="padding-left:4em">
	 * <table width="100%" border="1">
	 * <tr bgcolor="#EEEEFF">
	 *    <th width="100">エラーコード</th>
	 *    <th>原因、処置</th>
	 *  </tr>
	 *  <tr>
	 *    <td>notFoundErrorCodeプロパティに指定されたコード</td>
 	 *    <td>権限判定対象のオブジェクトを取得する事ができません。</td>
 	 *  </tr>
 	 *  <tr>
	 *    <td>accessErrorCodeプロパティに指定されたコード</td>
 	 *    <td>権限判定対象のオブジェクトへのアクセスが許可されていません。</td>
 	 *  </tr>
 	 * </table>
	 * <br>
	 * @since Ver1.0
	 * <br>
	 */
	public void check(Object target)  throws Exception {
		log.debug(AccessAuthorityCheckAdvice.class.getName() + " exceuted");

		// パラメータ変換
		ObjectDomain objectDomain = null;
		if (parameterConverter != null) {
			objectDomain = (ObjectDomain)parameterConverter.convert(target);
		}
		else{
			objectDomain = (ObjectDomain)target;
		}

		// 権限チェック
		boolean isAccessable = objectService.authorized(objectDomain, new AccessRoleTypeDomain(accessRoleTypeName));
		if(!isAccessable){
			// 存在チェック
			ObjectCriteria objectCriteria = new ObjectCriteria();
			objectCriteria.setAccessRoleType(null);
			MultipleCriteria<Long> mc = new MultipleCriteria<Long>();
			mc.add(objectDomain.getId());
			objectCriteria.setIds(mc);
			objectCriteria.setAttributeTypeListForMultiTargetMethod(new ArrayList<AttributeTypeDomain>());

			if(objectService.getList(objectCriteria).size() != 1){	// 存在しない場合
				//何もしない
			}
			else {													// 存在する場合
				throw new EIMException(accessErrorCode);
			}
		}
	}

	/**
	 * workspaceService.update()が処理されるときに動くAdvice関数です。<br>
	 * 組み込んだ対象のメソッドの特定の引数の値を元にアクセスが許可されているかのチェックを行います。
	 * @param target アクセスが許可されているか判定するための元となる情報
	 * @throws Exception
	 */
	public void workspaceAccessAuthorityCheck(Object target)  throws Exception {
		log.debug(AccessAuthorityCheckAdvice.class.getName() + " exceuted");

		// パラメータ変換
		ObjectDomain objectDomain = null;
		if (parameterConverter != null) {
			objectDomain = (ObjectDomain)parameterConverter.convert(target);
		}
		else{
			objectDomain = (ObjectDomain)target;
		}

		// Session
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMUser user = (EIMUser) sess.getUser();
		EIMObject object = ObjectUtils.getObjectById(sess, objectDomain.getId());

		if (Objects.isNull(object)) {
			throw new EIMException("EIM.ERROR.LOGIC.NOT_FOUND_WORKSPACE");
		}

		// 権限チェック
		boolean enabled = false; // 更新可能フラグ

		if (EIMXmlConfigAdminAuth.hasSpecifiedAuth(user, AppConstant.ADMIN_AUTH_ID_WORKSPACE)) {
			// ワークスペースの作成権限があるなら更新可
			enabled = true;
		} else {

			// オブジェクトに対して更新権限があるか
			boolean hasUpdateRole = false;
			// 下位フォルダ管理セキュリティ取得
			long sec_id = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"),
					Integer.MIN_VALUE);
			if (sec_id != Integer.MIN_VALUE) {
				// ワークスペースのセキュリティ&下位フォルダ管理セキュリティでチェック
				if (SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE) && AppSecurityUtils
						.authorizedLowFolderSecurity(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
					hasUpdateRole = true;
				}
			} else {
				// ワークスペースのセキュリティでチェック
				hasUpdateRole = SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE);
			}

			if (hasUpdateRole && WorkSpaceUtil.isWorkSpaceAdminUser(sess, user, object)) {
				// 更新権限があるかつワークスペースの責任者であるなら更新可
				enabled = true;
			}
		}

		if (object != null && enabled == false) {
			// ワークスペース責任者は更新可
			enabled = WorkSpaceUtil.isWorkSpaceAdminUser(sess, user, object);
		}

		// ここまで権限チェック
		if (enabled == false) {
			throw new EIMException("EIM.ERROR.LOGIC.NOACCESS");
		}
	}

	/**
	 * workspaceService.getById()が処理されるときに動くAdvice関数です。<br>
	 * 組み込んだ対象のメソッドの特定の引数の値を元にアクセスが許可されているかのチェックを行います。
	 * @param target アクセスが許可されているか判定するための元となる情報
	 * @throws Exception
	 */
	public void workspaceGetByIdAccessAuthorityCheck(Object target)  throws Exception {
		log.debug(AccessAuthorityCheckAdvice.class.getName() + " exceuted");

		// パラメータ変換
		ObjectDomain objectDomain = null;
		if (parameterConverter != null) {
			objectDomain = (ObjectDomain)parameterConverter.convert(target);
		}
		else{
			objectDomain = (ObjectDomain)target;
		}

		// Session
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMUser user = (EIMUser) sess.getUser();
		EIMObject object = ObjectUtils.getObjectById(sess, objectDomain.getId());

		if (Objects.isNull(object)) {
			throw new EIMException("EIM.ERROR.LOGIC.NOT_FOUND_WORKSPACE");
		}

		// 権限チェック
		boolean enabled = false; // 更新可能フラグ

		if (EIMXmlConfigAdminAuth.hasSpecifiedAuth(user, AppConstant.ADMIN_AUTH_ID_WORKSPACE)) {
			// ワークスペースの作成権限があるなら更新可
			enabled = true;
		} else {

			// オブジェクトに対して更新権限があるか
			boolean hasUpdateRole = false;
			// 下位フォルダ管理セキュリティ取得
			long sec_id = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"),
					Integer.MIN_VALUE);
			if (sec_id != Integer.MIN_VALUE) {
				// ワークスペースのセキュリティ&下位フォルダ管理セキュリティでチェック
				if (SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE) && AppSecurityUtils
						.authorizedLowFolderSecurity(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
					hasUpdateRole = true;
				}
			} else {
				// ワークスペースのセキュリティでチェック
				hasUpdateRole = SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE);
			}

			if (hasUpdateRole && WorkSpaceUtil.isWorkSpaceAdminUser(sess, user, object)) {
				// 更新権限があるかつワークスペースの責任者であるなら更新可
				enabled = true;
			}
		}

		if (object != null && enabled == false) {
			// ワークスペース責任者は更新可
			enabled = WorkSpaceUtil.isWorkSpaceAdminUser(sess, user, object);
		}

		// ここまで権限チェック
		if (enabled == false) {
			throw new EIMException("EIM.ERROR.LOGIC.NOACCESS");
		}
	}


	/**
	 * アクセスロールタイプ定義名称を取得します。
	 * @return アクセスロールタイプ定義名称
	 */
	public String getAccessRoleTypeName() {
	    return accessRoleTypeName;
	}

	/**
	 * アクセスロールタイプ定義名称を設定します。
	 * @param accessRoleTypeName アクセスロールタイプ定義名称
	 */
	public void setAccessRoleTypeName(String accessRoleTypeName) {
	    this.accessRoleTypeName = accessRoleTypeName;
	}

	/**
	 * アクセス権限が拒否時に発生するエラーのコードを取得します。
	 * @return アクセス権限が拒否時に発生するエラーのコード
	 */
	public String getAccessErrorCode() {
	    return accessErrorCode;
	}

	/**
	 * アクセス権限が拒否時に発生するエラーのコードを設定します。
	 * @param accessErrorCode アクセス権限が拒否時に発生するエラーのコード
	 */
	public void setAccessErrorCode(String accessErrorCode) {
	    this.accessErrorCode = accessErrorCode;
	}

	/**
	 * 権限チェック対象のオブジェクトが存在しなかった時に発生するエラーのコードを取得します。
	 * @return 権限チェック対象のオブジェクトが存在しなかった時に発生するエラーのコード
	 */
	public String getNotFoundErrorCode() {
	    return notFoundErrorCode;
	}

	/**
	 * 権限チェック対象のオブジェクトが存在しなかった時に発生するエラーのコードを設定します。
	 * @param notFoundErrorCode 権限チェック対象のオブジェクトが存在しなかった時に発生するエラーのコード
	 */
	public void setNotFoundErrorCode(String notFoundErrorCode) {
	    this.notFoundErrorCode = notFoundErrorCode;
	}

	/**
	 * パラメータコンバータを取得します。
	 * @return パラメータコンバータ
	 */
	public ParameterConverterPlugIn getParameterConverter() {
	    return parameterConverter;
	}

	/**
	 * パラメータコンバータを設定します。
	 * @param parameterConverter パラメータコンバータ
	 */
	public void setParameterConverter(ParameterConverterPlugIn parameterConverter) {
	    this.parameterConverter = parameterConverter;
	}

	/**
	 * 存在チェック、権限チェックに使用するオブジェクトServiceを取得します。
	 * @return 存在チェック、権限チェックに使用するオブジェクトService
	 */
	public ObjectService getObjectService() {
	    return objectService;
	}

	/**
	 * 存在チェック、権限チェックに使用するオブジェクトServiceを設定します。
	 * @param objectService 存在チェック、権限チェックに使用するオブジェクトService
	 */
	public void setObjectService(ObjectService objectService) {
	    this.objectService = objectService;
	}
}
