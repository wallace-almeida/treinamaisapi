package com.adontoApi.service;

import com.adontoApi.entity.User;
import com.adontoApi.entity.UserType;
import com.adontoApi.entity.dto.SignUpRequest;
import com.adontoApi.entity.dto.allUserDto;
import com.adontoApi.exception.NegocioException;
import com.adontoApi.repository.UserRepository;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

@Service
public class UserService {

	private final UserRepository repository;
    private  final PasswordEncoder passwordEncoder;

    public UserService(UserRepository repository, PasswordEncoder passwordEncoder) {
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
	
	public User create(SignUpRequest signUpRequest) {
       User user = new User();
       user.setName(signUpRequest.getName());
       user.setEmail(signUpRequest.getEmail());
       user.setPassword(passwordEncoder.encode(signUpRequest.getPassword()));
       user.setPhone(signUpRequest.getPhone());
       user.setCpfUser(signUpRequest.getCpfUser());

        // Define o userType como PACIENTE se não for informado
        user.setUserType(signUpRequest.getUserType() != null
                ? UserType.valueOf(signUpRequest.getUserType())
                : UserType.PACIENTE);

	/*if(user.getEmail()!= null) {
        if (repository.existsByEmail(user.getEmail()) ) {
            throw new NegocioException(String.format(
                    "Esse E-mail já foi cadastrado: %s. Favor inserir um e-mail diferente.", user.getEmail()
            ));
        } else if (repository.existsByCpfUser(user.getCpfUser())) {
            throw new NegocioException(String.format(
                    "Esse CPF já foi cadastrado:"
            ));


    }}*/
return repository.save(user);

}

    public Page<allUserDto> findAllUsers(Pageable pageable) {
        Page<User> users = repository.findAll(pageable);
        return users.map(user -> {
           allUserDto allUserDto = new allUserDto();
           BeanUtils.copyProperties(user, allUserDto);
           return allUserDto;
        });
    }
	/*
	@Transactional
	public void update(User supintegracao) {
		
		User aplicacaoDb = findByIdIntegracao(supintegracao.getIdIntegracao());
		supintegracao.setIdIntegracao(aplicacaoDb.getIdIntegracao());
		repository.save(supintegracao);
		
	}
	
	*/

}
