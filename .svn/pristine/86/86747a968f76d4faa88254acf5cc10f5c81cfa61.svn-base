package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import common.util.AppConstant;
import eim.bo.EIMObject;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.ObjectUtils;
import eim.util.UserUtils;
import jp.co.ctc_g.eim.app.document.business.util.AppMailUtils;
import jp.co.ctc_g.eim.framework.business.dao.StatusDao;
import jp.co.ctc_g.eim.framework.business.domain.AssignDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.AbstractUserDefGroupPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * 承認を依頼されたユーザ
 * ・現在の「承認依頼中」ステータスのアサイン先
 * 
 * 承認依頼通知の送信先としての使用を想定する。
 *
 */
public class UserDefAddressRequestApprovalPlugInImpl extends
		AbstractUserDefGroupPlugInImpl {

	protected StatusDao statusDao;
	
	@Override
	public List<UserDomain> getUserListByObject(ObjectDomain object)
			throws Exception {

		//現ステータスのアサイン先ユーザのリストを作成し、返却する。
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMObject obj = ObjectUtils.getObjectById(sess, object.getId());
		List<AssignDomain> assignList = statusDao.getAssignTaskListByStatusId(obj.getStatus().getId()); 

		List<UserDomain> userList = new ArrayList<UserDomain>();
		for (AssignDomain assign : assignList)
		{
			long taskUserId = assign.getOwner().getId();
			EIMUser user = UserUtils.getUserById(sess, taskUserId);
			UserDomain userDomain = new UserDomain(user);
			if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
				userList.add(userDomain);
			}
		}
		
		AppMailUtils.removeDuplicateUser(userList, false);
		
		return userList;
	}

	public void setStatusDao(StatusDao statusDao) {
		this.statusDao = statusDao;
	}
	
}
