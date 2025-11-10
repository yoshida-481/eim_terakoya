package jp.co.ctc_g.eim.app.document.common.aop.advice;

import java.util.Objects;

import jp.co.ctc_g.eim.framework2.business.domain.entity.GroupDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RoleDomain;
import jp.co.ctc_g.eim.framework2.business.service.GroupService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.RoleService;
import jp.co.ctc_g.eim.framework2.common.aop.advice.OperationHistoryAdvice;

public class GroupAndRoleOperationHistoryAdvice extends OperationHistoryAdvice {

	/**
	 * オブジェクトサービス
	 */
	private ObjectService objectService;
	/**
	 * グループサービス
	 */
	private GroupService groupService;
	/**
	 * ロールサービス
	 */
	private RoleService roleService;
	/**
	 * 上書き用操作対象情報A
	 */
	private long otherRecordInfoIdA = -1L;
	/**
	 * 上書き用操作対象情報B
	 */
	private long otherRecordInfoIdB = -1L;

	/**
	 * 組み込んだ対象のメソッドの特定の引数の値を元に操作履歴の出力を行います。<br>
	 * <br>
	 * 操作対象情報Aを元に操作履歴の出力を行います。<br>
	 *
	 * それぞれの引数に対し、ParameterConverterPlugInから適切なドメインクラスを取得し操作対象情報の出力を行います。<br>
	 * ParameterConverterPlugInの設定がない場合はメソッドの引数から取得した情報で操作対象情報の出力を行います。<br>
	 * operationHistoryServiceにて指定オブジェクトに変換ができない場合はException（オブジェクトの型が不明です。）をスローします。<br>
	 * <br>
	 *
	 * @param targetA 操作対象情報A
	 * <br>
	 * @since
	 * <br>
	 */
	public void createGroupCreateOperationHistory(Object targetA) throws Exception {

		// FWのドメインに変換する.
		GroupDomain groupDomain = (GroupDomain)targetA;

		// 親グループの有無で処理を分ける.
		if (Objects.isNull(groupDomain.getParent())) {
			this.create(targetA);

		} else {

			// 親グループを取得
			GroupDomain parentGroup = groupDomain.getParent();

			// recordInfoIdを上書きする.
			this.setRecordInfoIdA(otherRecordInfoIdA);
			this.setRecordInfoIdB(otherRecordInfoIdB);
			this.createWithB(parentGroup,targetA);
		}
	}

	/**
	 * 組み込んだ対象のメソッドの特定の引数の値を元に操作履歴の出力を行います。<br>
	 * <br>
	 * 操作対象情報Aを元に操作履歴の出力を行います。<br>
	 *
	 * それぞれの引数に対し、ParameterConverterPlugInから適切なドメインクラスを取得し操作対象情報の出力を行います。<br>
	 * ParameterConverterPlugInの設定がない場合はメソッドの引数から取得した情報で操作対象情報の出力を行います。<br>
	 * operationHistoryServiceにて指定オブジェクトに変換ができない場合はException（オブジェクトの型が不明です。）をスローします。<br>
	 * <br>
	 *
	 * @param targetA 操作対象情報A
	 * <br>
	 * @since
	 * <br>
	 */
	public void createGroupDeleteOperationHistory(Object targetA) throws Exception {

		// FWのドメインに変換し、IDしか設定されていないため再取得する.
		GroupDomain tmpGroupDomain = (GroupDomain)targetA;
		GroupDomain groupDomain = groupService.getById(tmpGroupDomain.getId());

		// 設定したidのグループが存在するかチェック
		if (Objects.isNull(groupDomain)) {
			return;
		}

		// 親グループの有無で処理を分ける.
		if (Objects.isNull(groupDomain.getParent())) {
			this.create(targetA);

		} else {

			// 親グループを取得
			GroupDomain parentGroup = groupDomain.getParent();

			// recordInfoIdを上書きする.
			this.setRecordInfoIdA(otherRecordInfoIdA);
			this.setRecordInfoIdB(otherRecordInfoIdB);
			this.createWithB(parentGroup,targetA);
		}

	}

	/**
	 * 組み込んだ対象のメソッドの特定の引数の値を元に操作履歴の出力を行います。<br>
	 * <br>
	 * 操作対象情報Aを元に操作履歴の出力を行います。<br>
	 *
	 * それぞれの引数に対し、ParameterConverterPlugInから適切なドメインクラスを取得し操作対象情報の出力を行います。<br>
	 * ParameterConverterPlugInの設定がない場合はメソッドの引数から取得した情報で操作対象情報の出力を行います。<br>
	 * operationHistoryServiceにて指定オブジェクトに変換ができない場合はException（オブジェクトの型が不明です。）をスローします。<br>
	 * <br>
	 *
	 * @param targetA 操作対象情報A
	 * <br>
	 * @since
	 * <br>
	 */
	public void createRoleCreateOperationHistory(Object targetA) throws Exception {

		// FWのドメインに変換する
		RoleDomain roleDomain = (RoleDomain)targetA;

		// 親ロールの有無で処理を分ける.
		if (Objects.isNull(roleDomain.getParent())) {
			this.create(targetA);

		} else {

			// 親ロールを取得
			RoleDomain parentRole = roleDomain.getParent();

			// recordInfoIdを上書きする.
			this.setRecordInfoIdA(otherRecordInfoIdA);
			this.setRecordInfoIdB(otherRecordInfoIdB);
			this.createWithB(parentRole,targetA);
		}
	}

	/**
	 * 組み込んだ対象のメソッドの特定の引数の値を元に操作履歴の出力を行います。<br>
	 * <br>
	 * 操作対象情報Aを元に操作履歴の出力を行います。<br>
	 *
	 * それぞれの引数に対し、ParameterConverterPlugInから適切なドメインクラスを取得し操作対象情報の出力を行います。<br>
	 * ParameterConverterPlugInの設定がない場合はメソッドの引数から取得した情報で操作対象情報の出力を行います。<br>
	 * operationHistoryServiceにて指定オブジェクトに変換ができない場合はException（オブジェクトの型が不明です。）をスローします。<br>
	 * <br>
	 *
	 * @param targetA 操作対象情報A
	 * <br>
	 * @since
	 * <br>
	 */
	public void createRoleDeleteOperationHistory(Object targetA) throws Exception {

		// FWのドメインに変換し、IDしか設定されていないため再取得する.
		RoleDomain tmpRoleDomain = (RoleDomain)targetA;
		RoleDomain roleDomain = roleService.getById(tmpRoleDomain.getId());

		// 設定したidのロールが存在するかチェック
		if (Objects.isNull(roleDomain)) {
			return;
		}

		// 親ロールの有無で処理を分ける.
		if (Objects.isNull(roleDomain.getParent())) {
			this.create(targetA);

		} else {

			// 親ロールを取得
			RoleDomain parentRole = roleDomain.getParent();

			// recordInfoIdを上書きする.
			this.setRecordInfoIdA(otherRecordInfoIdA);
			this.setRecordInfoIdB(otherRecordInfoIdB);
			this.createWithB(parentRole,targetA);
		}

	}

	/**
	 * 上書き用の操作対象情報Aを取得します。
	 * @return 上書き用操作対象情報A
	 */
	public long getOtherRecordInfoIdA() {
		return this.otherRecordInfoIdA;
	}

	/**
	 * 上書き用の操作対象情報Aを設定します。
	 * @param otherRecordInfoIdA 操作対象情報A
	 */
	public void setOtherRecordInfoIdA(long otherRecordInfoIdA) {
		this.otherRecordInfoIdA = otherRecordInfoIdA;
	}

	/**
	 * 上書き用の操作対象情報Bを取得します。
	 * @return 上書き用操作対象情報B
	 */
	public long getOtherRecordInfoIdB() {
		return this.otherRecordInfoIdB;
	}

	/**
	 * 上書き用の操作対象情報Bを設定します。
	 * @param otherRecordInfoIdB 操作対象情報B
	 */
	public void setOtherRecordInfoIdB(long otherRecordInfoIdB) {
		this.otherRecordInfoIdB = otherRecordInfoIdB;
	}

	/**
	 * 操作履歴出力に使用するオブジェクトServiceを取得します。
	 * @return 操作履歴出力に使用するオブジェクトService
	 */
	public ObjectService getObjectService() {
	   return objectService;
	}

	/**
	 * 操作履歴出力に使用するオブジェクトServiceを設定します。
	 * @param objectService 操作履歴出力に使用するオブジェクトService
	 */
	public void setObjectService(ObjectService objectService) {
	    this.objectService = objectService;
	}

	/**
	 * 操作履歴出力に使用するグループServiceを取得します。
	 * @return 操作履歴出力に使用するグループService
	 */
	public GroupService getGroupService() {
	    return groupService;
	}

	/**
	 * 操作履歴出力に使用するグループServiceを設定します。
	 * @param groupService セットする groupService
	 */
	public void setGroupService(GroupService groupService) {
		this.groupService = groupService;
	}

	/**
	 * 操作履歴出力に使用するロールServiceを取得します。
	 * @return 操作履歴出力に使用するロールService
	 */
	public RoleService getRoleService() {
	    return roleService;
	}

	/**
	 * 操作履歴出力に使用するロールServiceを設定します。
	 * @param roleService セットする roleService
	 */
	public void setRoleService(RoleService roleService) {
		this.roleService = roleService;
	}
}
