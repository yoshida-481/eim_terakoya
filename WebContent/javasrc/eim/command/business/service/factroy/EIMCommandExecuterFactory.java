package eim.command.business.service.factroy;

//import command.business.service.execute.EIMCommandCheckoutByUserExecuter;
import eim.command.business.service.execute.EIMCommandCheckinByUserExecuter;
import eim.command.business.service.execute.EIMCommandCheckoutByUserExecuter;
import eim.command.business.service.execute.EIMCommandCreateExecuter;
import eim.command.business.service.execute.EIMCommandDeleteExecuter;
import eim.command.business.service.execute.EIMCommandEncExecuter;
import eim.command.business.service.execute.EIMCommandExecuter;
import eim.command.business.service.execute.EIMCommandFindExecuter;
import eim.command.business.service.execute.EIMCommandGetAttrExecuter;
import eim.command.business.service.execute.EIMCommandGetByIdExecuter;
import eim.command.business.service.execute.EIMCommandGetExecuter;
import eim.command.business.service.execute.EIMCommandLoginExecuter;
import eim.command.business.service.execute.EIMCommandLsExecuter;
import eim.command.business.service.execute.EIMCommandMkdirExecuter;
import eim.command.business.service.execute.EIMCommandPutExecuter;
import eim.command.business.service.execute.EIMCommandRmExecuter;
import eim.command.business.service.execute.EIMCommandSelectExecuter;
import eim.command.business.service.execute.EIMCommandUpdateAttrExecuter;
import eim.command.business.service.execute.EIMCommandVerifyExecuter;
import eim.command.common.util.EIMCommandConstant;

/**
 * コマンド実行クラスfactory
 * 
 *
 */
public class EIMCommandExecuterFactory {
	
	/**
	 * コマンドごとのコマンド実行実装インスタンスを生成する。
	 * @param cmdStr
	 * @return
	 * @throws Exception
	 */
	public EIMCommandExecuter create(String cmdStr) throws Exception{
		EIMCommandExecuter commandExecuter = null;
		
		if (cmdStr.equals(EIMCommandConstant.GET)){
			commandExecuter = new EIMCommandGetExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.GET_BY_ID)){
			commandExecuter = new EIMCommandGetByIdExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.PUT)){
			commandExecuter = new EIMCommandPutExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.LS)){
			commandExecuter = new EIMCommandLsExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.UPDATE_ATTR)){
			commandExecuter = new EIMCommandUpdateAttrExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.FIND)){
			commandExecuter = new EIMCommandFindExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.GET_ATTR)){
			commandExecuter = new EIMCommandGetAttrExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.CHECK_IN_BY_USER)){
			commandExecuter = new EIMCommandCheckinByUserExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.CHECK_OUT_BY_USER)){
			commandExecuter = new EIMCommandCheckoutByUserExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.ENC)){
			commandExecuter = new EIMCommandEncExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.VERIFY)){
			commandExecuter = new EIMCommandVerifyExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.LOGIN)){
			commandExecuter = new EIMCommandLoginExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.SELECT)){
			commandExecuter = new EIMCommandSelectExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.DELETE)){
			commandExecuter = new EIMCommandDeleteExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.CREATE)){
			commandExecuter = new EIMCommandCreateExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.MKDIR)){
			commandExecuter = new EIMCommandMkdirExecuter();
		}
		else if (cmdStr.equals(EIMCommandConstant.RM)){
			commandExecuter = new EIMCommandRmExecuter();
		}
		else {
			throw new Exception();
		}

		return commandExecuter;
	}
}
