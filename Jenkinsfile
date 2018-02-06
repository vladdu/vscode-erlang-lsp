#!groovy

pipeline {
	agent any
	options {
		disableConcurrentBuilds()
		timestamps()
		skipDefaultCheckout()
		buildDiscarder(logRotator(numToKeepStr: '10'))
	}
	stages {
		stage('Checkout') {
			steps{
				retry(3) {
					timeout(time: 30, unit: 'SECONDS') {
						script {
							checkout()
						}
					}
				}
			}
		}

		stage('Compile') {
			steps{
				script {
					compile()
                    archive()
				}
			}
		}

	}
	//post {
		//always {
			//deleteDir()
		//}
	//}


}

///////////////////////////////////

def checkout() {
    deleteDir()
    checkout scm
    git_branch = env.BRANCH_NAME

    sh('git rev-parse HEAD > GIT_COMMIT')
    git_commit=readFile('GIT_COMMIT')
    short_commit=git_commit.take(6)

    currentBuild.setDescription("${git_branch} - ${short_commit}")
}

def compile() {
    sh "npm install"
    sh "vsce package"
}

def analyze1() {
    step([$class: 'WarningsPublisher', canComputeNew: false, canResolveRelativePaths: true, canRunOnFailed: true,
        consoleParsers: [[parserName: 'Erlang Compiler (erlc)']],
        excludePattern: '', healthy: '', includePattern: '', messagesPattern: '', unHealthy: ''])
    step([$class: 'TasksPublisher', canComputeNew: false, excludePattern: '**/_build/**/*.*', healthy: '', high: 'FIXME,XXX', low: '', normal: 'TODO', pattern: '**/*.erl,**/*.hrl', unHealthy: ''])
}

def archive() {
    step([$class: 'ArtifactArchiver', artifacts: "*.vsix", fingerprint: true])
}

