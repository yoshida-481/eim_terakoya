package jp.co.ctc_g.eim.admin.business.dao;

import java.util.List;
import java.util.Map;

import jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;

/**
*
* システム管理のユーザ操作に関する機能DAO
*
*/
public interface AdminUserDao {

	/**
	 * 指定したユーザの他言語名称を取得し、ユーザIDをキーとしたマップを返却する
	 * @param userDomainList ユーザドメインリスト
	 * @return ユーザIDをキーとしたOtherNameDomainリストのマップ
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	public Map<Long, List<OtherNameDomain>> getOtherNameListMapByIds(List<UserDomain> userDomainList) throws Exception;

	/**
	 * 指定したユーザの他言語名称を取得し、ユーザIDをキーとしたマップを返却する
	 * @param userDomainList ユーザドメインリスト
	 * @return ユーザIDをキーとしたOtherNameDomainリストのマップ
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	public Map<String, UserDomain> getListByCodes(List<String> userCodeList) throws Exception;

}
