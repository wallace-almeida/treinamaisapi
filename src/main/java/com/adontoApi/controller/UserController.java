package com.adontoApi.controller;

import java.util.List;

import com.adontoApi.controller.swagger.UserControllerSwagger;
import com.adontoApi.entity.User;
import com.adontoApi.entity.dto.SignUpRequest;
import com.adontoApi.repository.UserRepository;
import com.adontoApi.service.UserService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;


@RestController
@RequestMapping(path = "/api/", produces = MediaType.APPLICATION_JSON_VALUE)
public class UserController implements UserControllerSwagger {

	@Autowired
	private UserRepository repository;

	@Autowired
	private UserService service;
	
	
	//BLOCO GET
	/*@Override
	@GetMapping
	public Page<User> findAll(SupIntegracaoFilter filter, @PageableDefault(size = 10) Pageable pageable) {
		Page<User> areasistemicaPage = repository.findAll(SupIntegracaoSpec.withFilter(filter), pageable);
		return areasistemicaPage;
	}*/
	
	@Override
	@GetMapping("/allUser")
	public List<User> findAll() {
		List<User> listUser = repository.getListUser();
		return listUser;
	}
/*
	@Override
	public User create(User supintegracao) {
		return null;
	}

	@Override
	public void update(User supintegracao) {

	}*/
	

	

	
//	//BLOCO POST
@PostMapping("/signUp")
@ResponseStatus(code = HttpStatus.CREATED)
@Override
public User create(@RequestBody @Valid SignUpRequest signUpRequest) {
		return service.create(signUpRequest);
	}

	/*
//	//BLOCO PUT
	@Override
	@PutMapping("/atualizar")
	@ResponseStatus(code = HttpStatus.OK)	
	public void update(@Valid @RequestBody User supintegracao) {
		service.update(supintegracao);		
	}

	*/
}
