package com.adontoApi.controller;

import com.adontoApi.controller.swagger.UserControllerSwagger;
import com.adontoApi.entity.User;
import com.adontoApi.entity.dto.SignUpRequest;
import com.adontoApi.entity.dto.allUserDto;
import com.adontoApi.repository.UserRepository;
import com.adontoApi.service.UserService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
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
	
	@GetMapping("/allUser")
	@Override
	public ResponseEntity<Page<allUserDto>> findAll(
			@RequestParam(defaultValue = "0") int page,
			@RequestParam(defaultValue = "10") int size,
			@RequestParam(defaultValue = "name") String sortBy
	) {
		Pageable pageable = PageRequest.of(page, size, Sort.by(sortBy));
		Page<allUserDto> users = service.findAllUsers(pageable);
		return ResponseEntity.ok(users);
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
