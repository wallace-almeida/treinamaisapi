package com.autosenseapi.service;


import com.autosenseapi.repository.proprietario.UsuarioProprietarioRepository;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

@Service
public class UserService {

	private final UsuarioProprietarioRepository repository;
    private  final PasswordEncoder passwordEncoder;

    public UserService(UsuarioProprietarioRepository repository, PasswordEncoder passwordEncoder) {
        this.repository = repository;
        this.passwordEncoder = passwordEncoder;
    }
/*
	public User findByIdIntegracao(Long integracao) {
		return repository.getIntegracaoById(integracao);
	}


	
	public List<User> findAll() {
		return repository.getListIntegracao();
				
	}*/
	

	/*
	@Transactional
	public void update(User supintegracao) {
		
		User aplicacaoDb = findByIdIntegracao(supintegracao.getIdIntegracao());
		supintegracao.setIdIntegracao(aplicacaoDb.getIdIntegracao());
		repository.save(supintegracao);
		
	}
	
	*/

}
